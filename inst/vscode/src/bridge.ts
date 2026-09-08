import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

/**
 * Wire protocol version. Bumped only on breaking payload changes; it is
 * deliberately independent of both this extension's version and the R
 * package's version, so extension fixes can ship without a CRAN release.
 */
export const PROTOCOL = 1;

export interface RaveRequest {
  protocol: number;
  id: string;
  time?: string;
  pid?: number;
  /** Working directory of the requesting R session; used to pick a window. */
  wd?: string | null;
  command: string;
  params: Record<string, unknown>;
}

export interface RaveResponse {
  protocol: number;
  id: string;
  ok: boolean;
  time: string;
  windowId: string;
  error?: string;
  result?: Record<string, unknown>;
}

/**
 * Reimplementation of ravepipeline's `R_user_dir(package, "cache")`.
 * Must stay in step with R/filesys.R; see resolveBridgeDir() below.
 */
function rUserCacheDir(pkg: string): string {
  const home = os.homedir();
  const fromEnv = process.env.R_USER_CACHE_DIR || process.env.XDG_CACHE_HOME;

  let base: string;
  if (fromEnv && fromEnv.length > 0) {
    base = fromEnv;
  } else if (process.platform === "win32") {
    base = path.join(process.env.LOCALAPPDATA || path.join(home, "AppData", "Local"), "R", "cache");
  } else if (process.platform === "darwin") {
    base = path.join(home, "Library", "Caches", "org.R-project.R");
  } else {
    base = path.join(home, ".cache");
  }

  return path.join(base, "R", pkg);
}

/** Setting wins, then the env var R also honours, then the R cache default. */
export function resolveBridgeDir(): string {
  const configured = vscode.workspace.getConfiguration("rave").get<string>("bridgeDir");
  if (configured && configured.trim().length > 0) {
    return path.resolve(untilde(configured.trim()));
  }

  const fromEnv = process.env.RAVE_VSCODE_BRIDGE_DIR;
  if (fromEnv && fromEnv.trim().length > 0) {
    return path.resolve(untilde(fromEnv.trim()));
  }

  return path.join(rUserCacheDir("ravepipeline"), "vscode-bridge");
}

function untilde(p: string): string {
  return p === "~" || p.startsWith("~/") ? path.join(os.homedir(), p.slice(1)) : p;
}

export function requestsDir(root: string): string {
  return path.join(root, "requests");
}
export function responsesDir(root: string): string {
  return path.join(root, "responses");
}
export function windowsDir(root: string): string {
  return path.join(root, "windows");
}

export function ensureBridgeDirs(root: string): void {
  for (const d of [root, requestsDir(root), responsesDir(root), windowsDir(root)]) {
    fs.mkdirSync(d, { recursive: true });
  }
}

/**
 * Announce this window so R can tell a live extension host from none at all,
 * and route a request to the window that owns the relevant workspace.
 */
export function announceWindow(root: string, windowId: string): void {
  const payload = {
    protocol: PROTOCOL,
    windowId,
    version: currentVersion(),
    flavor: vscode.env.appName,
    appHost: vscode.env.appHost,
    remoteName: vscode.env.remoteName ?? null,
    pid: process.pid,
    time: new Date().toISOString(),
    workspaceFolders: (vscode.workspace.workspaceFolders ?? [])
      .filter((f) => f.uri.scheme === "file")
      .map((f) => f.uri.fsPath)
  };
  writeJsonAtomic(path.join(windowsDir(root), `${windowId}.json`), payload);
}

export function removeWindow(root: string, windowId: string): void {
  try {
    fs.unlinkSync(path.join(windowsDir(root), `${windowId}.json`));
  } catch {
    /* already gone */
  }
}

function currentVersion(): string {
  return vscode.extensions.getExtension("dipterix.rave-vscode-plugin")?.packageJSON?.version ?? "0.0.0";
}

export function writeResponse(root: string, response: RaveResponse): void {
  writeJsonAtomic(path.join(responsesDir(root), `${response.id}.json`), response);
}

/**
 * Write via a temp file in the same directory, then rename. Rename is atomic
 * on both POSIX and Windows within one filesystem, so R never observes a
 * half-written payload.
 */
export function writeJsonAtomic(target: string, value: unknown): void {
  const tmp = `${target}.tmp-${process.pid}-${Date.now()}`;
  fs.writeFileSync(tmp, JSON.stringify(value, null, 2), "utf8");
  fs.renameSync(tmp, target);
}

export function readRequest(file: string): RaveRequest | null {
  let raw: string;
  try {
    raw = fs.readFileSync(file, "utf8");
  } catch {
    return null;
  }
  if (raw.trim().length === 0) {
    return null;
  }
  try {
    const parsed = JSON.parse(raw) as RaveRequest;
    if (typeof parsed?.id !== "string" || typeof parsed?.command !== "string") {
      return null;
    }
    return parsed;
  } catch {
    return null;
  }
}

/**
 * Does this window own the request? An absent `wd` means "any window".
 * Matching on containment rather than equality lets a request from a
 * subdirectory of the workspace still find its home.
 */
export function windowOwnsRequest(request: RaveRequest): boolean {
  const wd = request.wd;
  if (!wd || wd.length === 0) {
    return true;
  }
  const folders = (vscode.workspace.workspaceFolders ?? []).filter((f) => f.uri.scheme === "file");
  if (folders.length === 0) {
    return true;
  }
  const target = path.resolve(wd);
  return folders.some((f) => {
    const root = path.resolve(f.uri.fsPath);
    const rel = path.relative(root, target);
    return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel));
  });
}
