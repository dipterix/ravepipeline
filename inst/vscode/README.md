# RAVE VSCode Plugin

Runs [`ravepipeline`](https://dipterix.org/ravepipeline/) jobs from R as native
VS Code tasks, so a background job started in R appears in the Terminal panel
with a name, live output, and a stop button — the counterpart of the RStudio
Jobs pane.

Works in Visual Studio Code and in [Positron](https://positron.posit.co/).

## Usage

Install the extension, then from R:

```r
job <- ravepipeline::start_job(
  function() { Sys.sleep(5); Sys.getpid() },
  method = "vscode_task"
)
ravepipeline::resolve_job(job)
```

If the extension is not running, `start_job()` falls back to a `callr`
background process, so nothing breaks — you just lose the task UI.

Every job appears as `RAVE-Task [ID: <name>]`. The name is also the task's
identity, so two jobs sharing one reuse a single terminal instead of opening a
new one each time:

```r
# both runs share one terminal, the second clearing the first
for (i in 1:2) {
  ravepipeline::start_job(function() Sys.sleep(2),
                          method = "vscode_task", name = "preprocess")
}
```

Naming a job that is still running is not an error — the second takes a
suffixed id (`preprocess#2`) and runs alongside in its own terminal.

Leave `name` unset and the job gets its own terminal, which closes itself when
the job ends instead of piling up. That happens whether the job succeeded or
failed, so give a name to any job whose output you may want to read afterwards.

The process id of a running task is written to the **RAVE Pipeline** output
channel; it cannot appear in the name, which VS Code fixes when the task is
created.

### Notifications

R can raise a notification in the window it is running in:

```r
ravepipeline::vscode_notify("Pipeline finished")

choice <- ravepipeline::vscode_notify(
  "Rebuild the pipeline?", level = "warning",
  actions = c("Rebuild", "Later"), wait = TRUE
)
```

With `wait = TRUE` the call blocks until the user picks a button and returns
its label.

## How it works

A VS Code extension has no inbound API, so R and the extension rendezvous
through a directory:

```
<ravepipeline cache>/vscode-bridge/
  windows/<windowId>.json   # this window announces itself here while active
  requests/<id>.json        # R writes one file per request
  responses/<id>.json       # the extension writes the outcome back
```

R only attempts the bridge when a live window file exists, and otherwise falls
back immediately rather than waiting out a timeout. When several windows are
open, a request is routed to the window whose workspace contains the
requesting session's working directory, and claimed with an atomic rename so
exactly one window handles it.

## Settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `rave.enabled` | `true` | Watch for job requests from R. |
| `rave.bridgeDir` | `""` | Override the rendezvous directory. Empty uses the `ravepipeline` cache location. |

Run **RAVE: Show Bridge Status** from the Command Palette to see the resolved
directory and whether the watcher is active.

## Development

```sh
npm install
npm run watch     # rebuild on change
npm run typecheck
npm run package   # produces rave-vscode-plugin.vsix
```

Press <kbd>F5</kbd> in VS Code to launch an Extension Development Host.

## License

MIT
