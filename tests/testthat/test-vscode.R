# Editor integration tests: they probe the host for installed applications,
# write to the bridge directory, and start background jobs. None of that
# belongs on a CRAN check machine.
testthat::skip_on_cran()

ravepipeline <- asNamespace("ravepipeline")

# Run `expr` with a temporary set of environment variables, restoring whatever
# was there before (including variables that were previously unset).
with_envvars <- function(vars, expr) {
  # Callers build these by appending overrides onto a "clear everything" base,
  # so the same name can appear twice; the later entry is the intended one.
  vars <- vars[!duplicated(names(vars), fromLast = TRUE)]

  names_ <- names(vars)
  previous <- Sys.getenv(names_, names = TRUE, unset = NA)

  set <- vars[!is.na(vars)]
  if (length(set)) { do.call(Sys.setenv, as.list(set)) }
  unset <- names_[is.na(vars)]
  if (length(unset)) { Sys.unsetenv(unset) }

  on.exit({
    restore <- previous[!is.na(previous)]
    if (length(restore)) { do.call(Sys.setenv, as.list(restore)) }
    gone <- names(previous)[is.na(previous)]
    if (length(gone)) { Sys.unsetenv(gone) }
  }, add = TRUE)

  force(expr)
}

# The editor variables leak in when the suite itself runs inside VSCode, so
# every detection test starts from a cleared slate.
clear_editor_vars <- c(
  RSTUDIO = NA, TERM_PROGRAM = NA, VSCODE_PID = NA, POSITRON = NA,
  VSCODE_GIT_ASKPASS_NODE = NA, VSCODE_NLS_CONFIG = NA,
  VSCODE_CODE_CACHE_PATH = NA, VSCODE_IPC_HOOK = NA
)


testthat::test_that("vscode_avail() detects the editor", {

  # A plain terminal is not an editor session
  with_envvars(clear_editor_vars, {
    testthat::expect_false(ravepipeline$vscode_avail()$available)
  })

  # The integrated terminal sets TERM_PROGRAM
  with_envvars(c(clear_editor_vars, TERM_PROGRAM = "vscode"), {
    testthat::expect_true(ravepipeline$vscode_avail()$available)
  })

  # The extension host sets VSCODE_PID instead
  with_envvars(c(clear_editor_vars, VSCODE_PID = "1234"), {
    testthat::expect_true(ravepipeline$vscode_avail()$available)
  })

  # RStudio has its own job runner and must never be mistaken for VSCode,
  # even though RStudio can host a terminal that sets TERM_PROGRAM
  with_envvars(c(clear_editor_vars, TERM_PROGRAM = "vscode", RSTUDIO = "1"), {
    testthat::expect_false(ravepipeline$vscode_avail()$available)
  })
})


testthat::test_that("vscode_flavor() separates Positron from VSCode", {
  with_envvars(c(POSITRON = NA), {
    testthat::expect_identical(
      ravepipeline$vscode_flavor("/Applications/Positron.app"), "positron")
    testthat::expect_identical(
      ravepipeline$vscode_flavor("/Applications/Visual Studio Code.app"), "vscode")
    testthat::expect_identical(
      ravepipeline$vscode_flavor(NA_character_), "vscode")
  })

  # Positron's own sessions announce themselves directly
  with_envvars(c(POSITRON = "1"), {
    testthat::expect_identical(ravepipeline$vscode_flavor(NA_character_), "positron")
  })
})


testthat::test_that("vscode_env_paths() pulls paths out of JSON values", {
  # VSCODE_NLS_CONFIG is a JSON document, not a path; the application path has
  # to be recovered from inside it, and may contain spaces.
  nls <- '{"locale":"en-us","defaultMessagesFile":"/Applications/Visual Studio Code.app/Contents/Resources/app/out/nls.messages.json"}'

  with_envvars(c(clear_editor_vars, VSCODE_NLS_CONFIG = nls), {
    paths <- ravepipeline$vscode_env_paths()
    testthat::expect_true(any(grepl(
      "^/Applications/Visual Studio Code\\.app/Contents", paths)))
  })
})


testthat::test_that("vscode_cli() prefers the configured launcher", {
  fake <- tempfile(fileext = ".sh")
  on.exit({ unlink(fake) }, add = TRUE)
  writeLines("#!/bin/sh", fake)

  old <- options(ravepipeline.vscode_cli = fake)
  on.exit({ options(old) }, add = TRUE)

  testthat::expect_identical(ravepipeline$vscode_cli(), fake)

  # A configured path that does not exist must not be returned
  options(ravepipeline.vscode_cli = tempfile())
  testthat::expect_false(identical(ravepipeline$vscode_cli(), fake))
})


testthat::test_that("vscode_bridge_dir() honours the override", {
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    testthat::expect_identical(ravepipeline$vscode_bridge_dir(), root)

    ravepipeline$vscode_bridge_dir(check = TRUE)
    for (sub in c("requests", "responses", "windows")) {
      testthat::expect_true(dir.exists(file.path(root, sub)))
    }
  })
})


testthat::test_that("vscode_live_windows() filters stale and mismatched windows", {
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    ravepipeline$vscode_bridge_dir(check = TRUE)
    windows <- file.path(root, "windows")

    workspace <- tempfile()
    dir.create(workspace, recursive = TRUE, showWarnings = FALSE)

    write_window <- function(id, protocol = 1L, folders = workspace) {
      ravepipeline$vscode_write_json(
        list(protocol = protocol, windowId = id, workspaceFolders = folders),
        file.path(windows, sprintf("%s.json", id))
      )
    }

    # Nothing announced yet
    testthat::expect_length(ravepipeline$vscode_live_windows(wd = workspace), 0L)

    write_window("live")
    testthat::expect_length(ravepipeline$vscode_live_windows(wd = workspace), 1L)

    # A window speaking a different protocol must be ignored rather than used
    write_window("wrong-protocol", protocol = 99L)
    ids <- vapply(ravepipeline$vscode_live_windows(wd = workspace),
                  function(w) { w$windowId }, "")
    testthat::expect_false("wrong-protocol" %in% ids)

    # A crashed extension host leaves its file behind; an unrefreshed file is
    # treated as dead so that R falls back instead of waiting for a reply
    stale <- file.path(windows, "live.json")
    Sys.setFileTime(stale, Sys.time() - ravepipeline$VSCODE_WINDOW_STALE_SECONDS - 60)
    testthat::expect_length(ravepipeline$vscode_live_windows(wd = workspace), 0L)
  })
})


testthat::test_that("vscode_live_windows() puts the owning window first", {
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    ravepipeline$vscode_bridge_dir(check = TRUE)

    mine <- tempfile()
    theirs <- tempfile()
    dir.create(mine, recursive = TRUE, showWarnings = FALSE)
    dir.create(theirs, recursive = TRUE, showWarnings = FALSE)

    for (w in list(list(id = "other", dir = theirs), list(id = "owner", dir = mine))) {
      ravepipeline$vscode_write_json(
        list(protocol = 1L, windowId = w$id, workspaceFolders = w$dir),
        file.path(root, "windows", sprintf("%s.json", w$id))
      )
    }

    ids <- vapply(ravepipeline$vscode_live_windows(wd = mine),
                  function(w) { w$windowId }, "")
    testthat::expect_identical(ids[[1]], "owner")

    # A sub-directory of the workspace still belongs to that window
    nested <- file.path(mine, "a", "b")
    dir.create(nested, recursive = TRUE, showWarnings = FALSE)
    ids <- vapply(ravepipeline$vscode_live_windows(wd = nested),
                  function(w) { w$windowId }, "")
    testthat::expect_identical(ids[[1]], "owner")
  })
})


testthat::test_that("vscode_request() gives up and cleans up when unanswered", {
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    response <- ravepipeline$vscode_request("runTask", timeout = 0.3)
    testthat::expect_null(response)

    # An abandoned request must not be left behind for a window that starts later
    testthat::expect_length(
      list.files(file.path(root, "requests"), pattern = "\\.json$"), 0L)
  })
})


testthat::test_that("vscode_extension_files() excludes build output", {
  # `file.copy(recursive = TRUE)` dereferences symlinks, so copying an existing
  # `node_modules` turns npm's `.bin` shims into plain files whose relative
  # `require()` no longer resolves. Only sources may be staged for a build.
  source_dir <- tempfile()
  dir.create(file.path(source_dir, "src"), recursive = TRUE)
  dir.create(file.path(source_dir, "node_modules", ".bin"), recursive = TRUE)
  dir.create(file.path(source_dir, "dist"), recursive = TRUE)
  dir.create(file.path(source_dir, "out"), recursive = TRUE)
  on.exit({ unlink(source_dir, recursive = TRUE, force = TRUE) }, add = TRUE)

  for (f in c("package.json", "package-lock.json", "tsconfig.json",
              "esbuild.js", "README.md", "CHANGELOG.md", "LICENSE",
              "already-built.vsix", "src/extension.ts",
              "node_modules/.bin/vsce", "dist/extension.js")) {
    writeLines("x", file.path(source_dir, f))
  }

  staged <- basename(ravepipeline$vscode_extension_files(source_dir))

  # Everything the build needs
  for (keep in c("package.json", "package-lock.json", "tsconfig.json",
                 "esbuild.js", "src", "README.md", "CHANGELOG.md", "LICENSE")) {
    testthat::expect_true(keep %in% staged, info = keep)
  }

  # Nothing that npm regenerates, and nothing that would break if copied
  for (drop in c("node_modules", "dist", "out", "already-built.vsix")) {
    testthat::expect_false(drop %in% staged, info = drop)
  }
})


testthat::test_that("vscode_task_label() is the same shape for every job", {
  # The label is fixed regardless of what the caller passed, so a RAVE task is
  # recognisable in the terminal panel
  testthat::expect_identical(ravepipeline$vscode_task_label("asd"),
                             "RAVE-Task [ID: asd]")
  testthat::expect_identical(ravepipeline$vscode_task_label("a1b2c3"),
                             "RAVE-Task [ID: a1b2c3]")
})


testthat::test_that("notification payloads keep the actions an array", {
  # `auto_unbox = TRUE` turns a length-one character vector into a bare string.
  # The extension spreads `actions` into the message's button arguments, so a
  # lone label has to survive as an array.
  path <- tempfile(fileext = ".json")
  on.exit({ unlink(path) }, add = TRUE)

  encode <- function(actions) {
    ravepipeline$vscode_write_json(
      list(actions = as.list(as.character(actions))), path)
    paste(readLines(path, warn = FALSE), collapse = "")
  }

  testthat::expect_match(encode("Only"), '"actions":\\["Only"\\]')
  testthat::expect_match(encode(c("A", "B")), '"actions":\\["A","B"\\]')
  testthat::expect_match(encode(NULL), '"actions":\\[\\]')
})


testthat::test_that("vscode_notify() returns at once with nobody listening", {
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    started <- Sys.time()
    result <- ravepipeline::vscode_notify("nobody is listening", wait = TRUE)
    elapsed <- as.numeric(Sys.time() - started, units = "secs")

    testthat::expect_null(result)

    # No window announced itself, so the timeout must never be waited out --
    # `wait = TRUE` alone would otherwise stall the caller for a minute
    testthat::expect_lt(elapsed, 5)

    # And nothing may be left behind for a window that starts later
    testthat::expect_length(
      list.files(file.path(root, "requests"), pattern = "\\.json$"), 0L)
  })
})


testthat::test_that("start_job(method='vscode_task') falls back to callr", {
  # This is what protects every user without the extension: no editor window is
  # listening, so the job must still run, and promptly.
  root <- tempfile()
  on.exit({ unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)

  with_envvars(c(RAVE_VSCODE_BRIDGE_DIR = root), {
    started <- Sys.time()
    job <- ravepipeline::start_job(function() { Sys.getpid() },
                                   method = "vscode_task")
    elapsed <- as.numeric(Sys.time() - started, units = "secs")

    result <- ravepipeline::resolve_job(job)

    testthat::expect_true(is.numeric(result))
    testthat::expect_false(identical(as.integer(result), Sys.getpid()))

    # Falling back must not wait out the bridge timeout
    testthat::expect_lt(elapsed, 30)
  })
})
