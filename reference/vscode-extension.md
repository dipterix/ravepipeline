# Companion editor extension for `VSCode` and `Positron`

Installs, removes, or reports on the extension that lets
[`start_job`](http://dipterix.org/ravepipeline/reference/rave-pipeline-jobs.md)
run a job as a native editor task, showing it in the terminal panel with
a name, live output, and a stop button.

Without the extension nothing breaks: `start_job` falls back to a
background `'callr'` process and only the editor integration is lost.

## Usage

``` r
install_vscode_extension(
  vsix = NULL,
  cli = NULL,
  ask = interactive(),
  force = FALSE
)

uninstall_vscode_extension(cli = NULL, ask = interactive())

vscode_bridge_status()
```

## Arguments

- vsix:

  optional path or `URL` to a packaged extension. When omitted, the
  extension is installed from the editor's marketplace, and failing
  that, built from the sources bundled with this package.

- cli:

  path to the editor's command-line launcher; by default it is detected,
  including inside the application bundle when it is absent from `PATH`.

- ask:

  whether to ask before changing the editor installation; default is
  `TRUE` in interactive sessions. Installing writes outside the R
  session's temporary directory, so a non-interactive call must opt in
  explicitly with `ask=FALSE`.

- force:

  whether to reinstall when the extension is already present

## Value

`install_vscode_extension` and `uninstall_vscode_extension` return
`TRUE` on success, invisibly. `vscode_bridge_status` returns a list
describing the detected editor and any live windows.

## Examples

``` r

# Report what has been detected; never changes anything
vscode_bridge_status()
#> VSCode bridge status:
#>   Editor detected : no
#>   Launcher (CLI)  : not found
#>   Bridge directory: /home/runner/.cache/R/ravepipeline/vscode-bridge
#>   Protocol        : 1
#>   Live windows    : 0
#>   Install with    : install_vscode_extension()

if (FALSE) { # \dontrun{

install_vscode_extension()

# Reload the editor window, then:
job <- start_job(function() { Sys.sleep(5); Sys.getpid() },
                 method = "vscode_task")
resolve_job(job)

} # }
```
