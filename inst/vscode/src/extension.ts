import * as crypto from "crypto";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";

import {
  PROTOCOL,
  RaveRequest,
  announceWindow,
  ensureBridgeDirs,
  readRequest,
  removeWindow,
  requestsDir,
  resolveBridgeDir,
  responsesDir,
  windowOwnsRequest,
  writeResponse
} from "./bridge";
import { notify } from "./commands/notify";
import { runTask } from "./commands/runTask";

/**
 * Dispatch table. Adding a capability is a new entry here plus a protocol
 * note -- no change to the transport.
 */
const HANDLERS: Record<string, (request: RaveRequest) => Promise<Record<string, unknown>>> = {
  runTask,
  notify
};

/** Safety net for missed fs.watch events, and for requests written before activation. */
const SWEEP_INTERVAL_MS = 2000;

/** Requests nobody claimed by now are abandoned; R has long since fallen back. */
const STALE_REQUEST_MS = 5 * 60 * 1000;

/**
 * How often the window file is refreshed. R treats a window as dead once the
 * file goes stale, so a crashed extension host costs R nothing rather than a
 * full request timeout. Must stay well under R's staleness threshold.
 */
const HEARTBEAT_MS = 30 * 1000;

let lastHeartbeat = 0;

const windowId = crypto.randomUUID();
let output: vscode.OutputChannel;
let bridgeRoot = "";
let watcher: fs.FSWatcher | undefined;
let sweepTimer: NodeJS.Timeout | undefined;
const handled = new Set<string>();

export function activate(context: vscode.ExtensionContext): void {
  output = vscode.window.createOutputChannel("RAVE Pipeline");
  context.subscriptions.push(output);

  context.subscriptions.push(
    vscode.commands.registerCommand("rave.showBridgeStatus", () => {
      output.show(true);
      output.appendLine(`Bridge directory : ${bridgeRoot}`);
      output.appendLine(`Window id        : ${windowId}`);
      output.appendLine(`Protocol         : ${PROTOCOL}`);
      output.appendLine(`Watching         : ${watcher !== undefined}`);
    })
  );

  if (!vscode.workspace.getConfiguration("rave").get<boolean>("enabled", true)) {
    output.appendLine("RAVE bridge disabled by setting 'rave.enabled'.");
    return;
  }

  start(context);

  // A changed bridge directory means a different rendezvous point; restart.
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration("rave.bridgeDir") || e.affectsConfiguration("rave.enabled")) {
        stop();
        if (vscode.workspace.getConfiguration("rave").get<boolean>("enabled", true)) {
          start(context);
        }
      }
    })
  );

  // Workspace folders are part of the announcement, so re-announce on change.
  context.subscriptions.push(
    vscode.workspace.onDidChangeWorkspaceFolders(() => {
      if (bridgeRoot) {
        safely(() => announceWindow(bridgeRoot, windowId));
      }
    })
  );

  // The child pid cannot go in the task name -- that is fixed when the task is
  // built, and the process starts later -- so record it where it can be found.
  context.subscriptions.push(
    vscode.tasks.onDidStartTaskProcess((e) => {
      if (!isRaveTask(e.execution.task)) {
        return;
      }
      output.appendLine(`${e.execution.task.name} started (PID ${e.processId}).`);
    }),
    vscode.tasks.onDidEndTaskProcess((e) => {
      if (!isRaveTask(e.execution.task)) {
        return;
      }
      output.appendLine(`${e.execution.task.name} exited (code ${e.exitCode ?? "unknown"}).`);
    })
  );
}

function isRaveTask(task: vscode.Task): boolean {
  return (task.definition as { type?: string })?.type === "rave";
}

export function deactivate(): void {
  stop();
}

function start(context: vscode.ExtensionContext): void {
  try {
    bridgeRoot = resolveBridgeDir();
    ensureBridgeDirs(bridgeRoot);
    announceWindow(bridgeRoot, windowId);
    lastHeartbeat = Date.now();
  } catch (e) {
    output.appendLine(`Failed to initialise bridge: ${describe(e)}`);
    return;
  }

  output.appendLine(`RAVE bridge ready at ${bridgeRoot} (window ${windowId}, protocol ${PROTOCOL}).`);

  const dir = requestsDir(bridgeRoot);
  try {
    watcher = fs.watch(dir, () => sweep());
    context.subscriptions.push({ dispose: () => watcher?.close() });
  } catch (e) {
    // Not fatal -- the sweep below still picks requests up, just less promptly.
    output.appendLine(`Falling back to polling; fs.watch failed: ${describe(e)}`);
  }

  sweepTimer = setInterval(() => sweep(), SWEEP_INTERVAL_MS);
  context.subscriptions.push({ dispose: () => clearInterval(sweepTimer) });

  sweep();
}

function stop(): void {
  watcher?.close();
  watcher = undefined;
  if (sweepTimer) {
    clearInterval(sweepTimer);
    sweepTimer = undefined;
  }
  if (bridgeRoot) {
    safely(() => removeWindow(bridgeRoot, windowId));
  }
}

function sweep(): void {
  if (!bridgeRoot) {
    return;
  }

  heartbeat();
  discardStaleResponses();

  const dir = requestsDir(bridgeRoot);
  let entries: string[];
  try {
    entries = fs.readdirSync(dir).filter((f) => f.endsWith(".json"));
  } catch {
    return;
  }

  for (const entry of entries) {
    const file = path.join(dir, entry);
    const request = readRequest(file);

    if (!request) {
      discardIfStale(file);
      continue;
    }
    if (handled.has(request.id)) {
      continue;
    }
    if (request.protocol !== PROTOCOL) {
      // Another window may speak this version; leave it, but don't let it rot.
      discardIfStale(file);
      continue;
    }
    if (!windowOwnsRequest(request)) {
      discardIfStale(file);
      continue;
    }

    // Atomic claim: several windows may see the same file, exactly one rename wins.
    const claim = `${file}.claim-${windowId}`;
    try {
      fs.renameSync(file, claim);
    } catch {
      continue;
    }

    handled.add(request.id);
    // Handlers own their response and their cleanup; a slow one -- an
    // interactive notification, say -- must not hold up the requests behind it.
    void handle(request, claim);
  }
}

async function handle(request: RaveRequest, claimedFile: string): Promise<void> {
  const handler = HANDLERS[request.command];

  try {
    if (!handler) {
      throw new Error(`Unknown command '${request.command}'`);
    }
    const result = await handler(request);
    writeResponse(bridgeRoot, {
      protocol: PROTOCOL,
      id: request.id,
      ok: true,
      time: new Date().toISOString(),
      windowId,
      result
    });
    output.appendLine(`Handled ${request.command} (${request.id}).`);
  } catch (e) {
    const error = describe(e);
    // R needs the failure promptly so it can fall back rather than wait.
    safely(() =>
      writeResponse(bridgeRoot, {
        protocol: PROTOCOL,
        id: request.id,
        ok: false,
        time: new Date().toISOString(),
        windowId,
        error
      })
    );
    output.appendLine(`Failed ${request.command} (${request.id}): ${error}`);
  } finally {
    safely(() => fs.unlinkSync(claimedFile));
  }
}

/** Refresh the window file so R can distinguish a live host from a crashed one. */
function heartbeat(): void {
  const now = Date.now();
  if (now - lastHeartbeat < HEARTBEAT_MS) {
    return;
  }
  lastHeartbeat = now;
  safely(() => announceWindow(bridgeRoot, windowId));
}

/** Responses nobody collected -- R timed out and stopped listening. */
function discardStaleResponses(): void {
  const dir = responsesDir(bridgeRoot);
  try {
    for (const entry of fs.readdirSync(dir).filter((f) => f.endsWith(".json"))) {
      discardIfStale(path.join(dir, entry));
    }
  } catch {
    /* directory not ready yet */
  }
}

function discardIfStale(file: string): void {
  try {
    if (Date.now() - fs.statSync(file).mtimeMs > STALE_REQUEST_MS) {
      fs.unlinkSync(file);
    }
  } catch {
    /* raced with another window */
  }
}

function safely(fn: () => void): void {
  try {
    fn();
  } catch {
    /* best effort */
  }
}

function describe(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}
