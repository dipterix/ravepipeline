# Index the reports shipped under a directory (normally `inst/reports` of an
# installed package). Returns the report names together with the paths they
# resolve to, so that a name shown to the user always maps back to one file.
report_index <- function(report_root) {
  empty <- list(root = character(0L), name = character(0L), path = character(0L))
  if (length(report_root) != 1L || is.na(report_root) || !nzchar(report_root)) {
    return(empty)
  }
  if (!dir.exists(report_root)) { return(empty) }

  # The character classes (rather than `ignore.case`) keep the pattern used to
  # find the files and the pattern used to strip the extension in lockstep
  rel <- list.files(
    report_root,
    pattern = "[.][Rr][Mm][Dd]$",
    all.files = FALSE,
    full.names = FALSE,
    recursive = TRUE,
    include.dirs = FALSE
  )

  list(
    root = report_root,
    name = sub("[.][Rr][Mm][Dd]$", "", rel),
    path = file.path(report_root, rel)
  )
}

#' @title Discover and build reports shipped with an R package
#' @name package-reports
#' @description
#' \code{pkg_available_reports} lists the \verb{R Markdown} reports that a
#' package ships; \code{pkg_build_report} renders one of them to a
#' self-contained \verb{HTML} file.
#'
#' @details
#' Reports live under the \code{reports} directory of an installed package
#' (\code{inst/reports} in its sources). Every \verb{Rmd} file found there,
#' including files in sub-directories, is a report. Its name is the path
#' relative to \code{reports} with the extension removed, so
#' \verb{inst/reports/qc/summary.Rmd} is the report \code{"qc/summary"}.
#' Names are matched ignoring case.
#'
#' Two optional style sheets are picked up automatically: a shared
#' \verb{common.css} at the root of \code{reports}, and a
#' \verb{<report>_styles.css} sitting next to the \verb{Rmd} file. Both are
#' copied into \code{build_path} and passed to
#' \code{\link[rmarkdown]{html_document}} as its \code{css} argument.
#'
#' The report is knitted in a new environment whose parent is the one above
#' the global environment. Objects the caller left in the global environment
#' are therefore invisible to the report, and so are packages attached after
#' the render begins: a report that needs \pkg{knitr} should call
#' \code{knitr::kable()} rather than rely on \code{library(knitr)} in a setup
#' chunk.
#'
#' @param package character; name of an installed package to look in.
#' @param report_name character; name of the report to build, as returned by
#'   \code{pkg_available_reports}. Matched ignoring case.
#' @param ... additional output options passed to
#'   \code{\link[rmarkdown]{html_document}}; these override the defaults set
#'   by this function.
#' @param theme character; \verb{Bootswatch} theme name, default
#'   \code{"flatly"}.
#' @param code_folding character; whether code blocks start folded, one of
#'   \code{"hide"} (default), \code{"show"}, or \code{"none"}.
#' @param self_contained logical; whether to bundle all dependencies into a
#'   single \verb{HTML} file, default \code{TRUE}.
#' @param quiet logical; whether to suppress the rendering progress messages,
#'   default \code{FALSE}.
#' @param build_path directory in which the report is built; defaults to a
#'   fresh \code{\link{tempfile}}. \strong{Any existing file or directory at
#'   this path is deleted recursively} before the build starts, so do not
#'   point it at a location whose contents matter.
#' @param params named list of report parameters, only forwarded when
#'   non-empty; the \verb{Rmd} file must declare a matching \code{params}
#'   entry in its \verb{YAML} header.
#'
#' @returns
#' \code{pkg_available_reports}: a character vector of report names, or
#' \code{character(0)} when the package ships no reports.
#'
#' \code{pkg_build_report}: the normalized path to the generated
#' \verb{HTML} file inside \code{build_path}.
#'
#' @examples
#'
#' # This package ships no reports, hence `character(0)`
#' pkg_available_reports("ravepipeline")
#'
#' \dontrun{
#'
#' # Requires a package that ships reports, plus a working `pandoc`
#' pkg_available_reports("ravecore")
#'
#' pkg_build_report("project-snapshot", package = "ravecore")
#'
#' }
#'
NULL

#' @rdname package-reports
#' @export
pkg_available_reports <- function(package) {
  report_index(system.file("reports", package = package))$name
}

#' @rdname package-reports
#' @export
pkg_build_report <- function(report_name, package, ...,
                             theme = "flatly", code_folding = "hide",
                             self_contained = TRUE, quiet = FALSE,
                             build_path = tempfile(),
                             params = list()) {

  # report_name <- "project-snapshot"
  # package = "ravecore"
  # build_path = tempfile()

  index <- report_index(system.file("reports", package = package))
  sel <- tolower(index$name) == tolower(report_name)

  if (!any(sel)) {
    if (length(index$name)) {
      available <- sprintf("Available reports: %s",
                           paste0('"', index$name, '"', collapse = ", "))
    } else {
      available <- "This package has no reports available."
    }

    stop(sprintf("Unable to find report \"%s\" under package `%s`. %s",
                 report_name, package, available))
  }

  report_name <- index$name[sel][[1]]
  report_rmd <- index$path[sel][[1]]

  report_css <- c(
    file.path(index$root, "common.css"),
    sub("[.][Rr][Mm][Dd]$", "_styles.css", report_rmd)
  )
  report_css <- report_css[file.exists(report_css)]

  # ensure build_path
  if (file.exists(build_path)) {
    unlink(build_path, recursive = TRUE)
  }

  dir_create2(build_path)

  # `report_name` may contain a sub-directory; the copy is always flat
  report_rmd2 <- file.path(build_path,
                           sprintf("%s.rmd", tolower(basename(report_name))))
  if (!file.copy(report_rmd, report_rmd2)) {
    stop(sprintf("Unable to copy report \"%s\" into the build directory: %s",
                 report_name, shQuote(build_path)))
  }

  if (length(report_css)) {
    lapply(report_css, function(f) {
      if (!file.copy(f, file.path(build_path, basename(f)))) {
        warning("Unable to copy style sheet into the build directory: ",
                shQuote(f))
      }
    })
  }

  output_options <- utils::modifyList(
    list(
      self_contained = self_contained,
      theme = theme,
      code_folding = code_folding,
      # relative to `knit_root_dir`
      css = if (length(report_css)) { basename(report_css) }
    ),
    list(...)
  )

  rmarkdown <- require_package("rmarkdown", return_namespace = TRUE)
  try(silent = TRUE, { register_pandoc() })

  render_args <- list(
    input = report_rmd2,
    output_format = "html_document",
    output_file = file.path(build_path, "report.html"),
    output_options = output_options,
    knit_root_dir = build_path,
    runtime = "static",
    clean = TRUE,
    quiet = quiet,
    envir = new.env(parent = parent.env(globalenv()))
  )
  # `render` errors when `params` is given for a report that declares none
  if (length(params)) {
    render_args$params <- params
  }

  do.call(rmarkdown$render, render_args)

  return(normalizePath(file.path(build_path, "report.html"), winslash = "/"))
}
