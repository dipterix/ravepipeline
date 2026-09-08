import * as vscode from "vscode";
import type { RaveRequest } from "../bridge";

interface RunTaskParams {
  /** Stable identity: same key, same terminal. */
  key?: string;
  /** Only for the detail line; the job folder R polls. */
  jobId?: string;
  /** Legacy display name, honoured when an older R sends no `key`. */
  name?: string;
  /** Set for a key nothing can reuse, so the terminal should not linger. */
  closeOnFinish?: boolean;
  program?: string;
  args?: string[];
  cwd?: string;
  env?: Record<string, string>;
}

/** Terminal tabs are narrow; a runaway name helps nobody. */
const MAX_KEY_LENGTH = 100;

/**
 * Run the job script as a native task, so it shows in the Terminal panel with
 * a name, live output, and a stop button -- the VS Code counterpart of the
 * RStudio Jobs pane.
 */
export async function runTask(request: RaveRequest): Promise<Record<string, unknown>> {
  const params = (request.params ?? {}) as RunTaskParams;

  const program = params.program;
  if (typeof program !== "string" || program.length === 0) {
    throw new Error("runTask requires a 'program' path");
  }

  const args = Array.isArray(params.args) ? params.args.map(String) : [];
  const requested = taskKey(params, request);
  const key = freeKey(requested);

  const execution = new vscode.ProcessExecution(program, args, {
    cwd: params.cwd,
    env: params.env
  });

  const scope = vscode.workspace.workspaceFolders?.length
    ? vscode.TaskScope.Workspace
    : vscode.TaskScope.Global;

  // The definition -- not the name -- is what VS Code hashes into a task
  // identity, and that identity is what makes a Dedicated panel reuse its
  // terminal rather than open another one.
  const task = new vscode.Task(
    { type: "rave", id: key },
    scope,
    displayName(key),
    "RAVE",
    execution
  );

  // The name is fixed once the task exists, so the volatile parts live here.
  task.detail = `job ${params.jobId ?? key} · R session ${request.pid ?? "?"}`;

  // Long-running by nature; R polls status.rds for completion.
  task.isBackground = true;
  task.presentationOptions = {
    reveal: vscode.TaskRevealKind.Always,
    panel: vscode.TaskPanelKind.Dedicated,
    // A reused terminal starts clean.
    clear: true,
    // Show the output without stealing the caret from the editor.
    focus: false,
    echo: false,
    // An unnamed job's terminal is never reused, so it would just accumulate.
    // Closing is unconditional -- a failed run's output goes with it, which is
    // why a job worth inspecting should be given a name.
    close: params.closeOnFinish === true
  };

  const running = await vscode.tasks.executeTask(task);

  return {
    taskId: request.id,
    taskKey: key,
    requestedKey: requested,
    taskName: running.task.name
  };
}

/** One shape for every RAVE task, whatever the caller asked to call it. */
function displayName(key: string): string {
  return `RAVE-Task [ID: ${key}]`;
}

/**
 * Identity of the task, in order of preference: what R asked for, the legacy
 * display name an older R sends instead, and finally the request id, which is
 * unique and so never shares a terminal.
 */
function taskKey(params: RunTaskParams, request: RaveRequest): string {
  const candidates = [params.key, stripLegacyLabel(params.name)];
  for (const candidate of candidates) {
    if (typeof candidate === "string") {
      const key = candidate.trim();
      if (key.length > 0) {
        return key.slice(0, MAX_KEY_LENGTH);
      }
    }
  }
  return request.id;
}

/** Older R composed its own label; keep the identity, drop the wrapper. */
function stripLegacyLabel(name: string | undefined): string | undefined {
  if (typeof name !== "string") {
    return undefined;
  }
  const match = /^RAVE-JobID:\s*(.+)$/.exec(name.trim());
  return match ? match[1].trim() : name;
}

function hasTaskKey(task: vscode.Task, key: string): boolean {
  const definition = task.definition as { type?: string; id?: string };
  return definition?.type === "rave" && definition.id === key;
}

/**
 * VS Code hands back the live execution instead of starting ours when a task
 * of the same identity is already running -- R would then wait on a status
 * file that never advances. Step aside onto a free key and let both run.
 * Bounded: there are finitely many live executions.
 */
function freeKey(requested: string): string {
  let key = requested;
  for (let n = 2; vscode.tasks.taskExecutions.some((e) => hasTaskKey(e.task, key)); n++) {
    key = `${requested}#${n}`;
  }
  return key;
}
