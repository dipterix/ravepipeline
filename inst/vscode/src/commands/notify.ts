import * as vscode from "vscode";
import type { RaveRequest } from "../bridge";

interface NotifyParams {
  message?: string;
  level?: string;
  actions?: string[];
  /** Wait for the user to click, so R learns which button they chose. */
  wait?: boolean;
  modal?: boolean;
}

/**
 * Raise an editor notification on behalf of R. The dispatch envelope was built
 * for exactly this: a new command, no protocol change.
 */
export async function notify(request: RaveRequest): Promise<Record<string, unknown>> {
  const params = (request.params ?? {}) as NotifyParams;

  const message = typeof params.message === "string" ? params.message.trim() : "";
  if (message.length === 0) {
    throw new Error("notify requires a 'message'");
  }

  const actions = Array.isArray(params.actions) ? params.actions.map(String) : [];
  const chosen = show(params.level, message, { modal: params.modal === true }, actions);

  // R is blocked on the response, so only hold it when it asked for the answer.
  if (params.wait !== true) {
    return { shown: true };
  }
  return { shown: true, action: (await chosen) ?? null };
}

// Called through `vscode.window` rather than a captured reference, so the
// namespace object is never detached from its own functions.
function show(
  level: string | undefined,
  message: string,
  options: vscode.MessageOptions,
  actions: string[]
): Thenable<string | undefined> {
  switch (level) {
    case "error":
      return vscode.window.showErrorMessage(message, options, ...actions);
    case "warning":
      return vscode.window.showWarningMessage(message, options, ...actions);
    default:
      return vscode.window.showInformationMessage(message, options, ...actions);
  }
}
