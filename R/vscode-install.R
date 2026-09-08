# Installer for the companion 'VSCode' extension shipped in 'inst/vscode'.

VSCODE_EXTENSION_ID <- "dipterix.rave-vscode-plugin"


#' @name vscode-extension
#' @title Companion editor extension for \verb{VSCode} and \verb{Positron}
#' @description
#' Installs, removes, or reports on the extension that lets
#' \code{\link{start_job}} run a job as a native editor task, showing it in the
#' terminal panel with a name, live output, and a stop button.
#'
#' Without the extension nothing breaks: \code{start_job} falls back to a
#' background \code{'callr'} process and only the editor integration is lost.
#'
#' @param vsix optional path or \verb{URL} to a packaged extension. When
#' omitted, the extension is installed from the editor's marketplace, and
#' failing that, built from the sources bundled with this package.
#' @param cli path to the editor's command-line launcher; by default it is
#' detected, including inside the application bundle when it is absent from
#' \code{PATH}.
#' @param ask whether to ask before changing the editor installation; default
#' is \code{TRUE} in interactive sessions. Installing writes outside the R
#' session's temporary directory, so a non-interactive call must opt in
#' explicitly with \code{ask=FALSE}.
#' @param force whether to reinstall when the extension is already present
#' @returns \code{install_vscode_extension} and
#' \code{uninstall_vscode_extension} return \code{TRUE} on success,
#' invisibly. \code{vscode_bridge_status} returns a list describing the
#' detected editor and any live windows.
#'
#' @examples
#'
#' # Report what has been detected; never changes anything
#' vscode_bridge_status()
#'
#' \dontrun{
#'
#' install_vscode_extension()
#'
#' # Reload the editor window, then:
#' job <- start_job(function() { Sys.sleep(5); Sys.getpid() },
#'                  method = "vscode_task")
#' resolve_job(job)
#'
#' }
#'
#' @export
install_vscode_extension <- function(vsix = NULL, cli = NULL,
                                     ask = interactive(), force = FALSE) {

  editor <- vscode_avail()
  cli <- vscode_resolve_cli(cli, editor)

  if (!nzchar(cli)) {
    message(
      "Could not find the VSCode command-line launcher.\n",
      "Pass it explicitly, for example:\n",
      "  install_vscode_extension(cli = \"",
      "/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code\")"
    )
    return(invisible(FALSE))
  }

  if (!isTRUE(force) && isTRUE(vscode_extension_installed(cli = cli))) {
    message(sprintf("Extension `%s` is already installed.", VSCODE_EXTENSION_ID))
    return(invisible(TRUE))
  }

  if (isTRUE(ask)) {
    if (!interactive()) {
      stop("`install_vscode_extension` needs `ask=FALSE` in a non-interactive session.")
    }
    answer <- utils::askYesNo(
      sprintf("Install the `%s` extension using `%s`?", VSCODE_EXTENSION_ID, cli),
      default = FALSE
    )
    if (!isTRUE(answer)) {
      message("Aborted.")
      return(invisible(FALSE))
    }
  }

  # Tier 1: an explicitly supplied package, local or remote.
  if (!is.null(vsix)) {
    return(invisible(vscode_install_vsix(vsix, cli = cli)))
  }

  # Tier 2: the editor's own marketplace, which also brings auto-updates.
  if (vscode_install_by_id(cli = cli)) {
    vscode_installed_message()
    return(invisible(TRUE))
  }

  # Tier 3: build from the sources shipped with this package.
  built <- tryCatch(vscode_build_vsix(), error = function(e) {
    message("Could not build the extension: ", conditionMessage(e))
    NULL
  })

  if (!is.null(built)) {
    return(invisible(vscode_install_vsix(built, cli = cli)))
  }

  # Tier 4: hand the user something they can run themselves.
  vscode_manual_instructions(cli)
  invisible(FALSE)
}

#' @rdname vscode-extension
#' @export
uninstall_vscode_extension <- function(cli = NULL, ask = interactive()) {
  editor <- vscode_avail()
  cli <- vscode_resolve_cli(cli, editor)

  if (!nzchar(cli)) {
    message("Could not find the editor command-line launcher.")
    return(invisible(FALSE))
  }

  if (isTRUE(ask)) {
    if (!interactive()) {
      stop("`uninstall_vscode_extension` needs `ask=FALSE` in a non-interactive session.")
    }
    answer <- utils::askYesNo(
      sprintf("Remove the `%s` extension?", VSCODE_EXTENSION_ID),
      default = FALSE
    )
    if (!isTRUE(answer)) {
      message("Aborted.")
      return(invisible(FALSE))
    }
  }

  status <- vscode_cli_run(cli, c("--uninstall-extension", VSCODE_EXTENSION_ID))
  invisible(identical(status, 0L))
}

#' @rdname vscode-extension
#' @export
vscode_bridge_status <- function() {
  editor <- vscode_avail()
  windows <- tryCatch(vscode_live_windows(), error = function(e) { list() })

  status <- list(
    available = editor$available,
    flavor = editor$flavor,
    app_dir = editor$app_dir,
    cli = editor$cli,
    bridge_dir = vscode_bridge_dir(),
    protocol = VSCODE_PROTOCOL,
    extension_id = VSCODE_EXTENSION_ID,
    live_windows = length(windows),
    workspaces = unlist(lapply(windows, function(w) { as.character(w$workspaceFolders) }))
  )

  structure(status, class = "ravepipeline_vscode_status")
}

#' @export
print.ravepipeline_vscode_status <- function(x, ...) {
  cat("VSCode bridge status:\n")
  cat(sprintf("  Editor detected : %s\n",
              if (isTRUE(x$available)) x$flavor else "no"))
  cat(sprintf("  Launcher (CLI)  : %s\n",
              if (nzchar(x$cli)) x$cli else "not found"))
  cat(sprintf("  Bridge directory: %s\n", x$bridge_dir))
  cat(sprintf("  Protocol        : %d\n", x$protocol))
  cat(sprintf("  Live windows    : %d\n", x$live_windows))
  if (length(x$workspaces)) {
    cat(sprintf("  Workspaces      : %s\n", paste(x$workspaces, collapse = ", ")))
  }
  if (!x$live_windows) {
    cat(sprintf("  Install with    : install_vscode_extension()\n"))
  }
  invisible(x)
}


# ---- Internals --------------------------------------------------------------

vscode_resolve_cli <- function(cli, editor) {
  if (length(cli) == 1 && is.character(cli) && !is.na(cli) && nzchar(cli)) {
    return(cli)
  }
  if (length(editor$cli) == 1 && nzchar(editor$cli)) {
    return(editor$cli)
  }
  # Not running inside the editor: try both flavors anyway.
  for (flavor in c("vscode", "positron")) {
    found <- vscode_cli(flavor = flavor)
    if (nzchar(found)) { return(found) }
  }
  ""
}

vscode_cli_run <- function(cli, args) {
  output <- suppressWarnings(
    system2(cli, args = shQuote(args), stdout = TRUE, stderr = TRUE)
  )

  status <- attr(output, "status")
  if (is.null(status)) { status <- 0L }
  attr(output, "status") <- NULL

  if (!identical(status, 0L)) {
    logger(paste(output, collapse = "\n"), level = "trace", calc_delta = FALSE)
  }
  status
}

vscode_extension_installed <- function(cli = NULL) {
  if (is.null(cli)) {
    cli <- vscode_resolve_cli(NULL, vscode_avail())
  }
  if (!nzchar(cli)) { return(FALSE) }

  output <- suppressWarnings(
    system2(cli, args = "--list-extensions", stdout = TRUE, stderr = FALSE)
  )
  isTRUE(any(tolower(trimws(output)) == tolower(VSCODE_EXTENSION_ID)))
}

vscode_install_by_id <- function(cli) {
  status <- vscode_cli_run(cli, c("--install-extension", VSCODE_EXTENSION_ID, "--force"))
  identical(status, 0L) && vscode_extension_installed(cli = cli)
}

vscode_install_vsix <- function(vsix, cli) {
  # A remote package has to land on disk before the launcher can read it.
  if (grepl("^https?://", vsix)) {
    destination <- file.path(tempdir(check = TRUE), basename(vsix))
    ok <- tryCatch({
      utils::download.file(vsix, destfile = destination, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("Could not download the extension: ", conditionMessage(e))
      FALSE
    })
    if (!ok) { return(FALSE) }
    vsix <- destination
  }

  if (!file.exists(vsix)) {
    message("No such extension package: ", vsix)
    return(FALSE)
  }

  status <- vscode_cli_run(cli, c("--install-extension",
                                  normalizePath(vsix, mustWork = TRUE), "--force"))
  if (!identical(status, 0L)) {
    message("The editor refused to install ", basename(vsix), ".")
    return(FALSE)
  }

  vscode_installed_message()
  TRUE
}

#' @title Build the extension from bundled sources
#' @description
#' Only usable where \verb{npm} is available: the package ships the extension's
#' sources but not its build output.
#' @returns Path to the built package.
#' @noRd
vscode_build_vsix <- function(source_dir = vscode_extension_source()) {
  if (!nzchar(source_dir) || !dir.exists(source_dir)) {
    stop("The bundled extension sources are missing.")
  }

  npm <- find_program("npm")
  if (!nzchar(npm)) {
    stop("`npm` is required to build the extension from source.")
  }

  # Build in a scratch copy: the installed package directory must stay untouched.
  build_dir <- file.path(tempdir(check = TRUE), "ravepipeline-vscode")
  unlink(build_dir, recursive = TRUE, force = TRUE)
  dir_create2(build_dir)
  file.copy(vscode_extension_files(source_dir), build_dir, recursive = TRUE)

  owd <- setwd(build_dir)
  on.exit({ setwd(owd) }, add = TRUE)

  npm_run(npm, c("install", "--no-audit", "--no-fund"))
  npm_run(npm, c("run", "package"))

  built <- list.files(build_dir, pattern = "\\.vsix$", full.names = TRUE)
  if (!length(built)) {
    stop("The build produced no extension package.")
  }
  built[[1]]
}

vscode_extension_source <- function() {
  system.file("vscode", package = "ravepipeline")
}

#' @title Extension files worth staging for a build
#' @description
#' An allowlist rather than an allow-everything copy. \code{file.copy} follows
#' symbolic links, so copying an existing \code{node_modules} rewrites npm's
#' \code{.bin} shims as plain files whose relative \code{require} no longer
#' resolves, and the build then fails inside a tool that looks installed.
#' Everything excluded here is regenerated by \verb{npm} anyway.
#' @returns Existing paths within \code{source_dir}, absolute.
#' @noRd
vscode_extension_files <- function(source_dir) {
  wanted <- c(
    "package.json", "package-lock.json", "tsconfig.json", "esbuild.js",
    "src", "README.md", "CHANGELOG.md", "LICENSE"
  )
  paths <- file.path(source_dir, wanted)
  paths[file.exists(paths)]
}

# Keep the output: a build failure is unactionable without the tool's message.
npm_run <- function(npm, args, tail_lines = 20L) {
  output <- suppressWarnings(
    system2(npm, args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(output, "status")
  if (is.null(status)) { status <- 0L }

  if (!identical(status, 0L)) {
    detail <- utils::tail(as.character(output), tail_lines)
    stop(sprintf(
      "`npm %s` failed:\n%s",
      paste(args, collapse = " "),
      paste(detail, collapse = "\n")
    ))
  }
  invisible(output)
}

vscode_installed_message <- function() {
  message(
    "Extension installed. Reload the editor window ",
    "(Command Palette > 'Developer: Reload Window') to activate it."
  )
}

vscode_manual_instructions <- function(cli) {
  message(
    "Could not install the extension automatically.\n",
    "Build and install it manually with:\n",
    sprintf("  cd %s\n", shQuote(vscode_extension_source())),
    "  npm install && npm run package\n",
    sprintf("  %s --install-extension rave-vscode-plugin.vsix\n", shQuote(cli))
  )
}
