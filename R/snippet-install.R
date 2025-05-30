#' @name rave-snippet
#' @title 'RAVE' code snippets
#' @description Run snippet code
#' @param topic snippet topic
#' @param local whether to use local snippets first before requesting online
#' repository
#' @param path for installing code snippets locally only; can be an R script,
#' a zip file, or a directory
#' @param force whether to force updating the snippets; default is true
#' @returns `load_snippet` returns snippet as a function, others return nothing
#' @examples
#'
#' # This example script requires running in an interactive session
#'
#' if(interactive()){
#'
#' # ---- Example 1: Install built-in pipeline snippets ------------
#' update_local_snippet(force = TRUE)
#'
#' # ---- Example 2: Install customized pipeline snippets ----------
#' snippets <- file.path(
#'   "https://github.com/rave-ieeg/rave-gists",
#'   "archive/refs/heads/main.zip",
#'   fsep = "/"
#' )
#' tempf <- tempfile(fileext = ".zip")
#' utils::download.file(url = snippets, destfile = tempf)
#'
#' install_snippet(tempf)
#'
#' }
#'
#' # ---- List snippets --------------------------------------------
#'
#' # list all topics
#' list_snippets()
#'
#'
#' # ---- Run snippets as functions --------------------------------
#'
#' topic <- "image-burn-contacts-to-t1"
#'
#' # check whether this example can run
#' # This snippet requires installing package `raveio`
#' # which is currently not on CRAN (soon it will)
#'
#' condition_met <- topic %in% list_snippets() &&
#'   (system.file(package = "raveio") != "")
#'
#' if( interactive() && condition_met ) {
#'
#'   snippet <- load_snippet(topic)
#'
#'   # Read snippet documentation
#'   print(snippet)
#'
#'
#'   results <- snippet(
#'     subject_code = "DemoSubject",
#'     project_name = "demo",
#'     save_path = NA,
#'     blank_underlay = FALSE
#'   )
#'
#'   plot(results)
#' }
#'
NULL

#' @rdname rave-snippet
#' @export
update_local_snippet <- function(force = TRUE) {
  root_path <- ravepipeline_cache_dir()
  snippet_path <- file.path(root_path, "rave-gists-main")
  if(!force && dir.exists(snippet_path)) {
    return(invisible())
  }

  dir_create2(tempdir())
  tmpfile <- tempfile(fileext = ".zip")
  utils::download.file(
    "https://github.com/rave-ieeg/rave-gists/archive/refs/heads/main.zip",
    destfile = tmpfile)
  on.exit({
    unlink(tmpfile)
  }, add = TRUE)
  install_snippet(tmpfile)
}

#' @rdname rave-snippet
#' @export
install_snippet <- function(path) {
  root_path <- ravepipeline_cache_dir()
  snippet_path <- file.path(root_path, "rave-gists-main")
  if( endsWith(tolower(path), ".zip") ) {
    dir_create2(tempdir())
    tmp_directory <- tempfile()
    utils::unzip(path, exdir = tmp_directory)
    on.exit({
      unlink(tmp_directory, recursive = TRUE)
    })
    tmp_inner <- list.dirs(tmp_directory, full.names = FALSE, recursive = FALSE)
    tmp_inner <- tmp_inner[!startsWith(tmp_inner, ".")]
    if(length(tmp_inner) > 0) {
      path <- file.path(tmp_directory, tmp_inner[[1]])
    } else {
      path <- tmp_directory
    }
  }
  path <- normalizePath(path, mustWork = TRUE)
  if( dir.exists(path) ) {
    files <- list.files(
      path,
      all.files = FALSE,
      full.names = FALSE,
      recursive = TRUE,
      include.dirs = FALSE,
      no.. = TRUE
    )
    lapply(files, function(file) {
      from <- file.path(path, file)
      to <- file.path(snippet_path, file)
      dir_create2(dirname(to))
      file.copy(from = from, to = to, overwrite = TRUE)
      return()
    })
  } else {
    file.copy(from = path,
              to = file.path(dir_create2(snippet_path), basename(path)),
              overwrite = TRUE)
  }
  return(invisible())
}


#' @rdname rave-snippet
#' @export
list_snippets <- function() {
  root <- ravepipeline_cache_dir("rave-gists-main")
  if(!dir.exists(root)) { return(character(0L)) }

  topics <- list.files(root, pattern = "\\.R", ignore.case = TRUE, include.dirs = FALSE, recursive = FALSE, no.. = TRUE, all.files = FALSE, full.names = FALSE)
  topics <- gsub("\\.R$", "", x = topics, ignore.case = TRUE)
  topics <- sort(topics)
  topics
}

#' @rdname rave-snippet
#' @export
load_snippet <- function(topic, local = TRUE) {

  fname <- sprintf("%s.R", topic)
  save_snippet <- FALSE
  if(!isFALSE(local)) {
    if(isTRUE(local)) {
      update_local_snippet(force = FALSE)
      path <- ravepipeline_cache_dir("rave-gists-main", fname)
    } else {
      path <- file.path(local, fname)
    }
    if(!startsWith(path, "https://") && !file.exists(path)) {
      warning("Cannot find local snippet [", topic, "]. Please make sure RAVE is up-to-date. Trying to download snippets from RAVE repository.")
      local <- FALSE
      save_snippet <- TRUE
    }
  }

  if(isFALSE(local)) {
    path <- sprintf("https://raw.githubusercontent.com/rave-ieeg/rave-gists/main/%s", fname)
  }

  # load scripts
  s <- trimws(readLines(path))

  # find documentation
  end_of_doc <- c(which(s == "#' END OF DOC"), length(s))[[1]]
  end_of_doc <- max(end_of_doc - 1L, 0L)

  docs <- s[seq_len(end_of_doc)]
  docs <- docs[startsWith(docs, "#'")]

  # get inputs
  params <- trimws(gsub("^#'", "", docs))
  params <- params[grepl("^@param [^\\ ]+ ", params)]
  params <- unlist(lapply(strsplit(params, " "), function(x){ x[[2]] }))

  params <- unique(c(params, "..."))

  args <- NULL
  missing_arg <- alist(params = )
  for(nm in params) {
    if(nm != "") {
      names(missing_arg) <- nm
      args <- c(args, missing_arg)
    }
  }

  fbody <- parse(text = c("{", s, "}"))[[1]]
  f <- new_function2(
    args = args,
    body = fbody,
    quote_type = "quote",
    env = new.env(parent = globalenv())
  )

  attr(f, "docs") <- docs
  attr(f, "args") <- params
  attr(f, "path") <- path
  attr(f, "topic") <- topic

  class(f) <- c("rave_snippet", class(f))

  if( save_snippet ) {
    snippet_path <- dir_create2(ravepipeline_cache_dir("rave-gists-main"))
    writeLines(s, con = file.path(snippet_path, basename(fname)))
  }

  f
}

#' @export
print.rave_snippet <- function(x, ...) {
  topic <- sprintf("<RAVE code snippet: [%s]>", attr(x, "topic"))
  path <- sprintf("Snippet path: %s", attr(x, "path"))

  docs <- gsub("^#'[ ]{0,1}", "", attr(x, "docs"))

  docs <- paste0(ifelse(startsWith(docs, "@"), "  ", "    "), docs)

  docs <- docs[docs != ""]

  usage <- sprintf("  @usage: snippet(%s)",
                   paste(attr(x, "args"), collapse = ", "))

  cat(paste(c(topic , path, docs, "", usage, ""), collapse = "\n"))
  return(invisible(x))
}
