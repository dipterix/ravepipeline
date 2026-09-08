# Change Log

## 0.2.0

- Added the `notify` command, so R can raise an editor notification through
  `ravepipeline::vscode_notify()`, optionally with buttons whose answer is
  returned to R.
- Tasks are now keyed on the job's name rather than on the request id, so two
  jobs sharing a name share one terminal instead of opening a new one each
  time. A name still in use by a running task is suffixed (`asd#2`) so both
  jobs run rather than the second silently joining the first.
- Every task is shown as `RAVE-Task [ID: <name>]`, whatever the caller passed.
  The job id and the calling R session appear in the task's detail line, and
  the child process id is written to the RAVE Pipeline output channel when the
  process starts.
- Requests no longer queue behind a slow handler, and responses R stopped
  waiting for are swept up rather than left behind.

## 0.1.0

- Initial release.
- Watches the `ravepipeline` bridge directory and runs requested job scripts as
  native VS Code tasks (`runTask`, wire protocol 1).
- Announces each window so R can detect a live extension host and route
  requests to the window owning the relevant workspace.
- Supported in Visual Studio Code and Positron.
