# `src_pipeline_path` is the source pipeline,
# export to `dst_pipeline_name` settings.yaml
pipeline_get_export_wizard <- function(dst_pipeline_name, src_pipeline_path) {

  # dst_pipeline_name <- "power_explorer"
  # src_pipeline_path <- "~/Dropbox (Personal)/projects/rave-pipelines/modules/power_clust/"

  export_wizard_path <- file.path(src_pipeline_path, "R", "import-export-wizard.R")

  if (!file.exists(export_wizard_path)) {
    return(NULL)
  }

  parse_env <- new.env(parent = globalenv())
  parse_env$pipeline <- pipeline_from_path(src_pipeline_path)
  source(export_wizard_path, local = parse_env, chdir = TRUE)
  export_wizard <- parse_env$.export_wizard

  if (!inherits(export_wizard, "fastmap2")) {
    return(NULL)
  }

  wizard <- export_wizard[[dst_pipeline_name]]

  if (!is.function(wizard)) {
    return(NULL)
  }

  return(wizard)
}

pipeline_get_import_wizard <- function(src_pipeline_name,
                                       pipeline_path = Sys.getenv("RAVE_PIPELINE", ".")) {

  # src_pipeline_name <- "power_explorer"
  # pipeline_path <- "~/Dropbox (Personal)/projects/rave-pipelines/modules/power_clust/"

  import_wizard_path <- file.path(pipeline_path, "R", "import-export-wizard.R")

  if (!file.exists(import_wizard_path)) {
    return(NULL)
  }

  parse_env <- new.env(parent = globalenv())
  parse_env$pipeline <- pipeline_from_path(pipeline_path)
  source(import_wizard_path, local = parse_env, chdir = TRUE)
  import_wizard <- parse_env$.import_wizard

  if (!inherits(import_wizard, "fastmap2")) {
    return(NULL)
  }

  wizard <- import_wizard[[src_pipeline_name]]

  if (!is.function(wizard)) {
    return(NULL)
  }

  return(wizard)
}

#' @title Translate pipeline settings between pipelines
#'
#' @description
#' Translate pipeline settings between pipelines using export and/or import
#' wizard functions defined in each pipeline's
#' \code{R/import-export-wizard.R} file. `pipeline_export_wizard` and
#' `pipeline_import_wizard` register those wizard functions.
#'
#' @details
#' Translation proceeds in up to two passes:
#' \enumerate{
#'   \item \strong{Export pass}: the source pipeline may declare an export wizard
#'     keyed by \code{dst_pipeline_name}; if present it converts the settings
#'     into the destination format.
#'   \item \strong{Import pass}: the destination pipeline may declare an import
#'     wizard keyed by the (possibly already-converted) source pipeline name;
#'     if present it applies an additional filter. This also handles the case
#'     where the destination pipeline defines a self-filter applied after the
#'     export pass.
#' }
#' At least one wizard must exist; otherwise an error is raised.
#'
#' @param src_pipeline_name character; name of the source pipeline whose
#'   settings are being translated.
#' @param dst_pipeline_name character; name of the destination pipeline to
#'   translate the settings into.
#' @param settings named list of settings to translate. If \code{NULL}
#'   (default), the current settings of \code{src_pipeline_name} are read
#'   automatically.
#' @param fun a function with signature \code{function(settings)} that performs
#'   the settings translation and returns the modified settings list.
#' @param pipeline_name character; the pipeline name this wizard handles, with
#'   context-dependent meaning:
#'   \itemize{
#'     \item In \code{pipeline_export_wizard}: the \strong{destination} pipeline
#'       name — the wizard is invoked when exporting the current pipeline's
#'       settings \emph{to} \code{pipeline_name} (corresponds to
#'       \code{dst_pipeline_name} in \code{pipeline_translate_settings}).
#'     \item In \code{pipeline_import_wizard}: the \strong{source} pipeline
#'       name — the wizard is invoked when importing settings \emph{from}
#'       \code{pipeline_name} into the current pipeline (corresponds to
#'       \code{src_pipeline_name} in \code{pipeline_translate_settings}).
#'   }
#' @param env environment in which to register the wizard. Defaults to the
#'   calling frame, i.e. the sourced \code{import-export-wizard.R} environment.
#'
#' @return
#' \describe{
#'   \item{\code{pipeline_translate_settings}}{A named list of translated
#'     settings compatible with \code{dst_pipeline_name}.}
#'   \item{\code{pipeline_export_wizard}, \code{pipeline_import_wizard}}{
#'     \code{fun}, invisibly. Called for the side effect of registering the
#'     wizard in \code{env}.}
#' }
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' # Translate settings from "pipelineA" to "pipelineB"
#' new_settings <- pipeline_translate_settings(
#'   src_pipeline_name = "pipelineA",
#'   dst_pipeline_name = "pipelineB"
#' )
#'
#' # To achieve this, you would define export and/or import wizards in the
#' # respective pipelines.
#'
#' # Option 1: Inside the source pipeline (pipelineA):
#' # file `R/import-export-wizard.R`, define an export wizard for pipelineB:
#'
#' pipeline_export_wizard(
#'   pipeline_name = "pipelineB",
#'   fun = function(settings_a) {
#'     settings_b$frequency_range <- settings_a$freq_range
#'     settings_b
#'   }
#' )
#'
#' # Option 2: Inside the destination pipeline (pipelineB):
#' # file `R/import-export-wizard.R`, define an import wizard for pipelineA:
#'
#' pipeline_import_wizard(
#'   pipeline_name = "pipelineA",
#'   fun = function(settings_a) {
#'     settings_b$frequency_range <- settings_a$freq_range
#'     settings_b
#'   }
#' )
#'
#' }
#'
#' @export
#' @rdname pipeline_translate_settings
pipeline_translate_settings <- function(
    src_pipeline_name,
    dst_pipeline_name,
    settings = NULL) {

  translated <- FALSE

  # src_pipeline_name <- "power_clust"
  # dst_pipeline_name <- "power_explorer"

  src_pipeline <- pipeline(src_pipeline_name)


  if (is.null(settings)) {
    settings <- src_pipeline$get_settings()
  }

  if (identical(src_pipeline_name, dst_pipeline_name)) {
    return(settings)
  }

  # Get export wizard from source pipeline
  export_wizard <- pipeline_get_export_wizard(
    src_pipeline_path = src_pipeline$pipeline_path,
    dst_pipeline_name = dst_pipeline_name
  )


  if (is.function(export_wizard)) {
    # export settings to pipeline with name `dst_pipeline_name`
    settings <- export_wizard(settings = settings)

    # settings has been converted from src_pipeline to dst_pipeline
    src_pipeline_name <- dst_pipeline_name
    translated <- TRUE
  }

  # Also check if dst_pipeline_name has an import wizard.
  # This handles two cases:
  #   1. No export wizard ran: dst imports directly from src
  #   2. Export wizard already ran (src_pipeline_name == dst_pipeline_name):
  #      dst applies its own self-filter on the already-converted settings
  dst_pipeline <- pipeline(dst_pipeline_name)

  import_wizard <- pipeline_get_import_wizard(
    src_pipeline_name = src_pipeline_name,
    pipeline_path = dst_pipeline$pipeline_path
  )

  if (is.function(import_wizard)) {
    # dst pipeline import settings from source pipeline name
    settings <- import_wizard(settings)
    translated <- TRUE
  }

  if (!translated) {
    stop(
      "Cannot export pipeline settings to ",
      sQuote(dst_pipeline_name),
      ". There is no import nor export wizard function."
    )
  }

  return(settings)

}

#' @export
#' @rdname pipeline_translate_settings
pipeline_export_wizard <- function(fun, pipeline_name, env = parent.frame()) {
  stopifnot("Input to `pipeline_export_wizard` must be a function" = is.function(fun))
  if (!"settings" %in% names(formals(fun))) {
    stop(
      "Input to `pipeline_export_wizard` must have signature ",
      "`function(settings) {...}`"
    )
  }

  if (!inherits(env$.export_wizard, "fastmap2")) {
    env$.export_wizard <- fastmap2()
  }

  env$.export_wizard[[pipeline_name]] <- fun
  invisible(fun)
}

#' @export
#' @rdname pipeline_translate_settings
pipeline_import_wizard <- function(fun, pipeline_name, env = parent.frame()) {
  stopifnot("Input to `pipeline_import_wizard` must be a function" = is.function(fun))
  if (!"settings" %in% names(formals(fun))) {
    stop(
      "Input to `pipeline_import_wizard` must have signature ",
      "`function(settings) {...}`"
    )
  }

  if (!inherits(env$.import_wizard, "fastmap2")) {
    env$.import_wizard <- fastmap2()
  }

  env$.import_wizard[[pipeline_name]] <- fun
  invisible(fun)
}
