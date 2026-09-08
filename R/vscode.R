# Bridge between an R session and the companion 'VSCode' extension.
#
# A 'VSCode' extension exposes no inbound API: an external process can only
# reach one through a file it watches. This file implements the R half of that
# rendezvous; the extension half lives in 'inst/vscode'.

# Wire protocol version, matched against what a window announces. Deliberately
# independent of both package versions so the extension can ship fixes to the
# marketplace without a CRAN release.
VSCODE_PROTOCOL <- 1L

# The extension refreshes its window file every 30 seconds. Allow generous
# slack for a busy extension host before calling a window dead.
VSCODE_WINDOW_STALE_SECONDS <- 120


#' @title Detect a 'VSCode'-family editor
#' @description
#' Determines whether the current R session is running inside \verb{VSCode} or
#' \verb{Positron}, and where that editor is installed.
#' @returns A list with elements \code{available}, \code{flavor},
#' \code{app_dir}, and \code{cli}.
#' @noRd
vscode_avail <- function() {
  none <- list(available = FALSE, flavor = NA_character_,
               app_dir = NA_character_, cli = "")

  # RStudio and Workbench take precedence; they have their own job runner.
  if (nzchar(Sys.getenv("RSTUDIO"))) { return(none) }

  # `TERM_PROGRAM` is what the integrated terminal sets and what the 'VSCode'
  # R extension itself gates on; `VSCODE_PID` covers the extension host.
  in_vscode <- identical(Sys.getenv("TERM_PROGRAM"), "vscode") ||
    nzchar(Sys.getenv("VSCODE_PID"))

  if (!in_vscode) { return(none) }

  app_dir <- vscode_app_dir()

  list(
    available = TRUE,
    flavor = vscode_flavor(app_dir),
    app_dir = app_dir,
    cli = vscode_cli(flavor = vscode_flavor(app_dir), app_dir = app_dir)
  )
}

# `POSITRON` is set in Positron's own R sessions but not in every context that
# embeds one, so fall back to the name of the application directory.
vscode_flavor <- function(app_dir = vscode_app_dir()) {
  if (identical(Sys.getenv("POSITRON"), "1")) { return("positron") }
  if (length(app_dir) == 1 && !is.na(app_dir) &&
      grepl("positron", app_dir, ignore.case = TRUE)) {
    return("positron")
  }
  "vscode"
}

# Several `VSCODE_*` variables embed the application path, but not all of them
# are plain paths: `VSCODE_NLS_CONFIG` is a JSON document. Extract path-like
# runs first, then reason about them.
vscode_env_paths <- function() {
  raw <- c(
    # Injected into integrated terminals by the built-in git extension
    Sys.getenv("VSCODE_GIT_ASKPASS_NODE"),
    # Present in the extension host
    Sys.getenv("VSCODE_NLS_CONFIG"),
    Sys.getenv("VSCODE_CODE_CACHE_PATH"),
    Sys.getenv("VSCODE_IPC_HOOK")
  )
  raw <- raw[nzchar(raw)]
  if (!length(raw)) { return(character(0L)) }

  # Absolute paths may contain spaces ("Application Support"), so stop only at
  # characters that cannot appear in one.
  unlist(lapply(raw, function(x) {
    regmatches(x, gregexpr("/[^\"',:]+", x))[[1]]
  }), use.names = FALSE)
}

# Recover the editor's installation directory.
vscode_app_dir <- function() {
  candidates <- vscode_env_paths()
  if (!length(candidates)) { return(NA_character_) }

  if (identical(get_os(), "darwin")) {
    # A bundled path looks like `/Applications/Positron.app/Contents/...`;
    # `\\.app` cannot match the plain `app` directory nested further down.
    for (candidate in candidates) {
      m <- regmatches(candidate, regexpr("^.*?\\.app(?=/|$)", candidate, perl = TRUE))
      if (length(m) == 1 && dir.exists(m)) { return(m) }
    }
    # Only the user-data directory was available; it is named after the app.
    for (candidate in candidates) {
      m <- regmatches(
        candidate,
        regexpr("(?<=/Application Support/)[^/]+", candidate, perl = TRUE)
      )
      if (length(m) == 1) {
        bundle <- file.path("/Applications", sprintf("%s.app", m))
        if (dir.exists(bundle)) { return(bundle) }
      }
    }
    return(NA_character_)
  }

  for (candidate in candidates) {
    # Walk up until a directory containing `bin/code` appears.
    dir <- dirname(candidate)
    for (i in seq_len(6)) {
      if (file.exists(file.path(dir, "bin", "code"))) { return(dir) }
      parent <- dirname(dir)
      if (identical(parent, dir)) { break }
      dir <- parent
    }
  }
  NA_character_
}

#' @title Locate the editor command-line launcher
#' @description
#' The launcher is frequently absent from \code{PATH} (installing it is an
#' opt-in step on 'macOS'), so fall back to the copy inside the application
#' bundle.
#' @returns Path to the launcher, or \code{""} when none is found, matching
#' the contract of \code{find_program}.
#' @noRd
vscode_cli <- function(flavor = "vscode", app_dir = NA_character_) {

  configured <- getOption("ravepipeline.vscode_cli", "")
  if (length(configured) == 1 && is.character(configured) &&
      !is.na(configured) && nzchar(configured) && file.exists(configured)) {
    return(configured)
  }

  os <- get_os()

  # Derived from the running editor: always the right one when available.
  if (length(app_dir) == 1 && !is.na(app_dir) && nzchar(app_dir)) {
    derived <- switch(
      os,
      "darwin" = file.path(app_dir, "Contents", "Resources", "app", "bin", "code"),
      "windows" = file.path(app_dir, "bin", "code.cmd"),
      file.path(app_dir, "bin", "code")
    )
    if (file.exists(derived)) { return(derived) }
  }

  for (program in vscode_cli_names(flavor)) {
    found <- find_program(program)
    if (length(found) == 1 && is.character(found) && !is.na(found) && nzchar(found)) {
      return(found)
    }
  }

  for (path in vscode_cli_paths(flavor, os)) {
    if (file.exists(path)) { return(path) }
  }

  ""
}

# Table-driven so another fork is a data change rather than new logic.
vscode_cli_names <- function(flavor) {
  switch(flavor, "positron" = c("positron", "code"), c("code", "code-insiders"))
}

vscode_cli_paths <- function(flavor, os) {
  app <- switch(flavor, "positron" = "Positron", "Visual Studio Code")
  switch(
    os,
    "darwin" = file.path("/Applications", sprintf("%s.app", app),
                         "Contents", "Resources", "app", "bin", "code"),
    "windows" = {
      local_app <- Sys.getenv("LOCALAPPDATA")
      roots <- c(
        if (nzchar(local_app)) file.path(local_app, "Programs", app),
        file.path(Sys.getenv("ProgramFiles"), app)
      )
      file.path(roots, "bin", "code.cmd")
    },
    c(
      sprintf("/usr/share/%s/bin/code", tolower(gsub(" ", "-", app, fixed = TRUE))),
      "/usr/bin/code",
      "/usr/local/bin/code",
      "/snap/bin/code"
    )
  )
}


# ---- Bridge directory -------------------------------------------------------

#' @title Directory shared with the 'VSCode' extension
#' @description
#' Defaults to a sub-directory of the package cache, which the extension
#' recomputes independently. An override must be applied on both sides.
#' @noRd
vscode_bridge_dir <- function(check = FALSE) {
  configured <- getOption("ravepipeline.vscode_bridge_dir", "")
  if (!(length(configured) == 1 && is.character(configured) &&
        !is.na(configured) && nzchar(configured))) {
    configured <- Sys.getenv("RAVE_VSCODE_BRIDGE_DIR")
  }

  path <- if (nzchar(configured)) {
    configured
  } else {
    file.path(R_user_dir("ravepipeline", "cache"), "vscode-bridge")
  }

  if (check) {
    dir_create2(file.path(path, "requests"))
    dir_create2(file.path(path, "responses"))
    dir_create2(file.path(path, "windows"))
  }
  path
}

# Plain JSON in both directions: the other end is JavaScript, so the
# `serializeJSON` form used elsewhere in the package is not applicable here.
vscode_write_json <- function(x, path) {
  # Write then rename so a watcher never observes a half-written payload.
  tmp <- sprintf("%s.tmp-%s", path, Sys.getpid())
  writeLines(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", force = TRUE),
    con = tmp
  )
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Unable to write bridge payload: ", path)
  }
  invisible(path)
}

vscode_read_json <- function(path) {
  tryCatch(
    jsonlite::fromJSON(readLines(path, warn = FALSE), simplifyVector = TRUE),
    error = function(e) { NULL }
  )
}


#' @title List live editor windows
#' @description
#' Each active window writes and periodically refreshes a file announcing
#' itself. Their absence means the extension is not running, which lets
#' \code{start_job} fall back immediately instead of waiting out a timeout.
#' @returns A list of window descriptors, best match first.
#' @noRd
vscode_live_windows <- function(wd = getwd()) {
  dir <- file.path(vscode_bridge_dir(), "windows")
  if (!dir.exists(dir)) { return(list()) }

  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) { return(list()) }

  now <- Sys.time()
  windows <- lapply(files, function(file) {
    info <- file.info(file)
    age <- as.numeric(difftime(now, info$mtime, units = "secs"))

    # A crashed extension host leaves its file behind; ignore anything the
    # heartbeat has stopped refreshing.
    if (is.na(age) || age > VSCODE_WINDOW_STALE_SECONDS) { return(NULL) }

    window <- vscode_read_json(file)
    if (!is.list(window) || !length(window$windowId)) { return(NULL) }
    if (!isTRUE(window$protocol == VSCODE_PROTOCOL)) { return(NULL) }

    window$age <- age
    window
  })

  windows <- windows[vapply(windows, is.list, FALSE)]
  if (!length(windows)) { return(list()) }

  # Prefer a window whose workspace contains the calling session, so that a
  # request lands where the user is actually looking.
  owns <- vapply(windows, function(window) {
    vscode_window_owns(window, wd)
  }, FALSE)

  c(windows[owns], windows[!owns])
}

vscode_window_owns <- function(window, wd) {
  folders <- window$workspaceFolders
  if (!length(folders)) { return(FALSE) }

  target <- normalizePath(wd, winslash = "/", mustWork = FALSE)
  folders <- normalizePath(as.character(folders), winslash = "/", mustWork = FALSE)

  any(vapply(folders, function(folder) {
    identical(target, folder) ||
      startsWith(target, paste0(folder, "/"))
  }, FALSE))
}


#' @title Send a request to the extension and wait for its reply
#' @returns The parsed response, or \code{NULL} on timeout.
#' @noRd
vscode_request <- function(command, params = list(), wd = getwd(), timeout = 10) {
  root <- vscode_bridge_dir(check = TRUE)
  request_id <- uuid::UUIDgenerate(use.time = TRUE, output = "string")

  payload <- list(
    protocol = VSCODE_PROTOCOL,
    id = request_id,
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    pid = Sys.getpid(),
    wd = normalizePath(wd, winslash = "/", mustWork = FALSE),
    command = command,
    params = params
  )

  vscode_write_json(payload, file.path(root, "requests", sprintf("%s.json", request_id)))

  vscode_await_response(root, request_id, timeout = timeout)
}

vscode_await_response <- function(root, request_id, timeout = 10) {
  path <- file.path(root, "responses", sprintf("%s.json", request_id))
  deadline <- Sys.time() + timeout

  while (Sys.time() < deadline) {
    if (file.exists(path)) {
      response <- vscode_read_json(path)
      unlink(path)
      if (is.list(response)) { return(response) }
      return(NULL)
    }
    Sys.sleep(0.05)
  }

  # Nobody answered: drop the request so it cannot be picked up later.
  unlink(file.path(root, "requests", sprintf("%s.json", request_id)))
  NULL
}


# ---- Job runner -------------------------------------------------------------

#' @title Run a prepared job as an editor task
#' @description
#' Sibling of \code{start_job_rs}. Falls back to \code{start_job_callr}
#' whenever the bridge cannot be used, so callers always get a running job.
#' @noRd
start_job_vscode <- function(fun, fun_args = list(), packages = NULL,
                             workdir = NULL, name = NULL, digest_key = NULL,
                             envvars = NULL, log_path = NULL,
                             timeout = 10, ...) {

  fallback <- function(reason) {
    if (!is.null(reason)) {
      logger("Running job via `callr` instead of a VSCode task: ", reason,
             level = "trace", calc_delta = FALSE)
    }
    start_job_callr(
      fun = fun, fun_args = fun_args, packages = packages, workdir = workdir,
      digest_key = digest_key, envvars = envvars, log_path = log_path
    )
  }

  wd <- workdir
  if (!(length(wd) == 1 && is.character(wd) && !is.na(wd) && dir.exists(wd))) {
    wd <- getwd()
  }

  # Cheap check first: no live window means no point preparing a job at all.
  windows <- tryCatch(vscode_live_windows(wd = wd), error = function(e) { list() })
  if (!length(windows)) {
    return(fallback("no VSCode window is running the RAVE extension"))
  }

  job_id <- prepare_job(
    fun = fun, fun_args = fun_args, packages = packages, workdir = workdir,
    digest_key = digest_key, envvars = envvars, log_path = log_path,
    # The task terminal is the point of this backend, so stream to it as well
    tee_console = TRUE
  )
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  # A caller-supplied name is the reuse key: same name, same terminal. Without
  # one the job id keeps every run on its own terminal.
  task_key <- if (length(name)) { trimws(as.character(name)[[1]]) } else { job_id }
  if (is.na(task_key) || !nzchar(task_key)) { task_key <- job_id }

  # An unnamed job's key is unique to that run, so its terminal can never be
  # reused; closing it on exit is what stops them piling up.
  close_on_finish <- identical(as.character(task_key), as.character(job_id))

  response <- tryCatch(
    vscode_request(
      command = "runTask",
      wd = wd,
      timeout = timeout,
      params = list(
        key = task_key,
        jobId = job_id,
        closeOnFinish = close_on_finish,
        # Older extensions ignore `key` and display `name` verbatim; sending
        # the composed label keeps them showing the right thing.
        name = vscode_task_label(task_key),
        # Same R installation as this session, not whatever is on PATH.
        program = vscode_rscript(),
        args = c("--no-save", "--no-restore", "--no-echo", script_path),
        cwd = wd,
        # The extension host's library paths may differ from this session's.
        env = list(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
      )
    ),
    error = function(e) { structure(list(ok = FALSE, error = conditionMessage(e))) }
  )

  if (!is.list(response) || !isTRUE(response$ok)) {
    remove_job(job_id)
    reason <- if (is.list(response) && length(response$error)) {
      as.character(response$error)[[1]]
    } else {
      "the VSCode extension did not respond"
    }
    return(fallback(reason))
  }

  # The extension adjusts the key when one is already taken by a running task,
  # or is too long for a terminal tab. Say what it became, or the terminal's
  # title is unexplainable.
  actual <- response$result$taskKey
  if (length(actual) && !identical(as.character(actual)[[1]], task_key)) {
    logger("The VSCode task for this job is named '", as.character(actual)[[1]],
           "' rather than '", task_key, "'.", level = "trace", calc_delta = FALSE)
  }

  structure(job_id, path = job_root)
}

# One shape for every RAVE task, whatever the caller asked to call it. The
# extension composes this itself; it is sent for the benefit of older ones.
vscode_task_label <- function(key) {
  sprintf("RAVE-Task [ID: %s]", key)
}

vscode_rscript <- function() {
  exe <- if (identical(get_os(), "windows")) { "Rscript.exe" } else { "Rscript" }
  path <- file.path(R.home("bin"), exe)
  if (file.exists(path)) { return(path) }
  # Should not happen, but a bare name still resolves through PATH.
  exe
}


# ---- Notifications ----------------------------------------------------------

#' @title Show a notification in the editor
#' @description
#' Raises a \verb{VSCode} or \verb{Positron} notification from R through the
#' companion extension, see \code{\link{install_vscode_extension}}. Returns at
#' once when no live editor window is listening, so it is safe to call
#' anywhere.
#' @param message text to show; a length-one character, or a vector whose
#' elements are shown on separate lines
#' @param level severity of the notification; one of \code{'info'},
#' \code{'warning'}, or \code{'error'}
#' @param actions optional character vector of button labels
#' @param wait whether to wait for the user to choose an action and return the
#' label they clicked; default is \code{FALSE}, returning as soon as the
#' notification has been raised
#' @param modal whether the notification blocks the editor until it is
#' dismissed; default is \code{FALSE}
#' @param timeout seconds to wait for the editor to answer; the default is 5,
#' or 60 when \code{wait=TRUE}
#' @returns The label the user clicked when \code{wait=TRUE}, or \code{NA} when
#' they dismissed the notification without choosing; otherwise \code{TRUE},
#' invisibly. \code{NULL} invisibly when no editor window answered.
#'
#' @examples
#'
#' \dontrun{
#'
#' vscode_notify("Pipeline finished")
#'
#' choice <- vscode_notify("Rebuild the pipeline?", level = "warning",
#'                         actions = c("Rebuild", "Later"), wait = TRUE)
#' if (identical(choice, "Rebuild")) { message("rebuilding") }
#'
#' }
#'
#' @export
vscode_notify <- function(message, level = c("info", "warning", "error"),
                          actions = NULL, wait = FALSE, modal = FALSE,
                          timeout = NULL) {
  level <- match.arg(level)
  message <- paste(as.character(message), collapse = "\n")
  wait <- isTRUE(wait)
  if (is.null(timeout)) { timeout <- if (wait) { 60 } else { 5 } }

  # Cheap check first: with nobody listening, waiting out the timeout would
  # stall the caller for nothing.
  windows <- tryCatch(vscode_live_windows(), error = function(e) { list() })
  if (!length(windows)) { return(invisible(NULL)) }

  response <- tryCatch(
    vscode_request(
      command = "notify",
      timeout = timeout,
      params = list(
        message = message,
        level = level,
        # A list keeps a lone label an array rather than a bare string once
        # `auto_unbox = TRUE` has been through it.
        actions = as.list(as.character(actions)),
        wait = wait,
        modal = isTRUE(modal)
      )
    ),
    error = function(e) { NULL }
  )

  if (!is.list(response) || !isTRUE(response$ok)) { return(invisible(NULL)) }
  if (!wait) { return(invisible(TRUE)) }

  action <- response$result$action
  if (!length(action) || is.na(action)) { return(NA_character_) }
  as.character(action)[[1]]
}
