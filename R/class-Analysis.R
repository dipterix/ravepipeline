# Validates a step function: `x` must be a function (or `NULL` when
# `allow_null` is true) whose formals include `arg_names`; a function with
# `...` accepts any argument
check_function_args <- function(x, name, allow_null = FALSE, arg_names = NULL) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.function(x)) {
    if (allow_null) {
      stop(sprintf("%s must be either `NULL` or a function", name), call. = FALSE)
    } else {
      stop(sprintf("%s must be a function", name), call. = FALSE)
    }
  }
  # `args` also works for primitive functions, and returns `NULL` for the
  # special ones that cannot be called with named arguments
  fun_args <- args(x)
  formal_names <- if (is.function(fun_args)) names(formals(fun_args)) else character(0L)
  if (length(arg_names) && !"..." %in% formal_names) {
    missing_arg_names <- arg_names[!arg_names %in% formal_names]
    if (length(missing_arg_names)) {
      stop(sprintf(
        "%s requires function arguments %s. The following arguments are missing: %s",
        name, paste(sprintf("`%s`", arg_names), collapse = ", "),
        paste(sprintf("`%s`", missing_arg_names), collapse = ", ")
      ), call. = FALSE)
    }
  }
  return(invisible(x))
}

# Whether every element of list `x` has a non-empty name; an empty list counts
is_named_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (!length(x)) {
    return(TRUE)
  }
  nms <- names(x)
  length(nms) == length(x) && !anyNA(nms) && all(nzchar(nms))
}

check_is_pipeline <- function(pipeline) {
  if (!inherits(pipeline, "PipelineTools")) {
    stop("`pipeline` must be a RAVE pipeline [PipelineTools] object.", call. = FALSE)
  }
  invisible(pipeline)
}

get_shiny_root_session <- function(session) {
  if (!is.environment(session) || !is.function(session$rootScope)) {
    stop("`session` must be a shiny session", call. = FALSE)
  }
  session$rootScope()
}


#' Modular analysis unit for 'RAVE' pipelines
#'
#' @description
#' A light-weight container that describes one analysis as a handful of plain
#' functions. The analysis never stores a pipeline: the 'RAVE' dashboard (the
#' controller) passes the pipeline, the \pkg{shiny} session, or the values of
#' the prerequisite pipeline targets to the methods that need them. Methods
#' whose names start with \code{@} are called by the controller (the
#' dashboard or the pipeline), not by analysis developers.
#'
#' When a pipeline is compiled, every analysis defined at the top level of a
#' \verb{R/shared-*.R} script becomes the pipeline target named
#' \code{results_target_name}, plus the target
#' \verb{analysis_cleaned_inputs_<name>} when the analysis has a custom
#' pipeline collector. The pipeline settings file must contain the key
#' \code{inputs_settings_name}; its value can start as an empty list.
#'
#' @examples
#' \dontrun{
#'
#' # 1. R/shared-analysis.R -- define the analysis; adapted from the
#' #    streamline collision detection of the 'RAVE' 3D viewer module
#' analysis <- RAVEPipelineAnalysis$new(
#'   name = "streamline_collision_detection",
#'   namespace = "custom_3d_viewer",
#'   description = "Streamline collision detection"
#' )
#'
#' # An input function receives `inputId` without the namespace, and gives
#' # the input `ns(inputId)`
#' analysis$set_input_ui("mode_x", function(inputId, ns, restored_inputs) {
#'   shiny::selectInput(
#'     inputId = ns(inputId),
#'     label = "Mode for ROI objects",
#'     choices = c("auto", "volume", "pointcloud", "surface"),
#'     selected = restored_inputs$mode_x %||% "auto"
#'   )
#' })
#' analysis$set_input_ui("radius", function(inputId, ns, restored_inputs) {
#'   shiny::tagList(
#'     shiny::numericInput(
#'       inputId = ns(inputId),
#'       label = "Radius (mm)",
#'       value = restored_inputs$radius %||% 0,
#'       min = 0,
#'       step = 0.1
#'     ),
#'     # `conditionalPanel()` adds the namespace itself, so its condition
#'     # refers to the input by `inputId`
#'     shiny::conditionalPanel(
#'       condition = sprintf("input['%s'] > 0", inputId),
#'       ns = ns,
#'       shiny::helpText("ROI objects are expanded by this radius.")
#'     )
#'   )
#' })
#'
#' # The module server uses the same identifiers
#' analysis$set_shiny_server(function(input, output, session) {
#'   shiny::observeEvent(input[[analysis$get_id("mode_x")]], {
#'     # a point cloud has no volume, so give it a positive radius
#'     if (input[[analysis$get_id("mode_x")]] == "pointcloud") {
#'       shiny::updateNumericInput(session, analysis$get_id("radius"), value = 1)
#'     }
#'   })
#' })
#'
#' # The preprocess step gathers what the analysis needs, including the
#' # pipeline targets it declares
#' analysis$set_preprocess(
#'   pipeline_targets = c("loaded_brain_info", "analysis_objects"),
#'   preprocess_func = function(value, pipeline_targets) {
#'     list(
#'       brain = pipeline_targets$loaded_brain_info$brain,
#'       objects = pipeline_targets$analysis_objects,
#'       mode_x = value$mode_x %||% "auto",
#'       radius = value$radius %||% 0
#'     )
#'   }
#' )
#'
#' # `detect_collision()` stands for the detection code, which lives in
#' # another `R/shared-*.R` script of the module
#' analysis$set_analyze(function(value, options) {
#'   detect_collision(value$brain, value$objects,
#'                    mode_x = value$mode_x, radius = value$radius)
#' })
#' analysis$set_visualize(function(value, options) {
#'   print(value)
#' })
#'
#' # 2. settings.yaml -- the key that holds the saved inputs, empty at first
#' #    analysis_inputs_streamline_collision_detection: []
#'
#' # 3. The dashboard -- render the inputs with the values saved in the
#' #    pipeline
#' pipeline <- pipeline("custom_3d_viewer")
#' analysis$render_inputs(pipeline)
#'
#' # In the dashboard server function: start the analysis server; to run,
#' # save the inputs from the session and build the analysis result
#' analysis$`@shiny_server`(session)
#' analysis$run(pipeline, session = session, visualization_method = "html")
#'
#' # Without the dashboard, run with the inputs saved in the pipeline
#' analysis$run(pipeline)
#' }
#'
#' @export
RAVEPipelineAnalysis <- R6::R6Class(
  classname = "RAVEPipelineAnalysis",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .name = character(0L),
    .namespace = character(0L),
    .ui = NULL,
    .server = NULL,
    .collect_from_shiny = NULL,
    .collect_from_pipeline = NULL,
    .store_to_pipeline = NULL,
    .preprocess = NULL,
    .pipeline_targets = character(0L),
    .analyze = NULL,
    .visualize = NULL,
    .options = NULL,

    # The values of the declared pipeline targets, from the named list
    # `pipeline_targets`
    .pick_pipeline_targets = function(pipeline_targets) {
      target_names <- private$.pipeline_targets
      missing_names <- target_names[!target_names %in% names(pipeline_targets)]
      if (length(missing_names)) {
        stop(
          sprintf(
            "Analysis '%s' requires the values of pipeline targets %s. The following targets are missing: %s",
            private$.name,
            paste(sprintf("`%s`", target_names), collapse = ", "),
            paste(sprintf("`%s`", missing_names), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      as.list(pipeline_targets)[target_names]
    }
  ),
  public = list(
    #' @field description a short text describing the analysis
    description = character(0L),

    #' @description Set options one key at a time; this is how to change
    #' individual options, since \code{analysis$options$key <- value} is not
    #' reliable on an active binding
    #' @param ...,.list named options; each replaces the whole option of the
    #' same name (a nested list is not merged), and a \code{NULL} value is
    #' kept as \code{NULL}. \code{.list} takes precedence over \code{...} for
    #' the same name
    #' @param .clear_first whether to remove all existing options first
    #' @returns The analysis object itself, invisibly
    set_option = function(..., .list = list(), .clear_first = FALSE) {
      new_options <- c(list(...), as.list(.list))
      if (!is_named_list(new_options)) {
        stop("All options must be named", call. = FALSE)
      }
      options <- if (.clear_first) list() else private$.options
      options[names(new_options)] <- new_options
      private$.options <- options
      invisible(self)
    },

    #' @description Constructor
    #' @param name analysis name, a single string of letters, digits, and
    #' underscores that starts with a letter; used as the prefix of the
    #' input identifiers
    #' @param namespace \pkg{shiny} module namespace under which the inputs
    #' are rendered, usually the module ID; a single non-empty string
    #' @param description a short text describing the analysis; default is
    #' the name with underscores replaced by spaces
    initialize = function(
      name,
      namespace,
      description = gsub("[_]+", " ", name)
    ) {
      if (
        !is.character(name) ||
          length(name) != 1L ||
          !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", name)
      ) {
        stop(
          "`name` must be a single string of letters, digits, and underscores, starting with a letter",
          call. = FALSE
        ) # nolint: line_length_linter.
      }
      if (
        !is.character(namespace) ||
          length(namespace) != 1L ||
          is.na(namespace) ||
          !nzchar(namespace)
      ) {
        stop("`namespace` must be a single non-empty string", call. = FALSE)
      }
      private$.name <- name
      private$.namespace <- namespace
      private$.ui <- list()
      private$.options <- list()
      self$description <- description
    },

    #' @description Get the identifier of an input or output element
    #' @param id input or output name, such as the \code{input_name} passed
    #' to \code{set_input_ui}
    #' @param with_namespace whether to add the \pkg{shiny} namespace prefix;
    #' default is false, which gives the identifier used inside the module
    #' server (for example \code{input[[id]]} or \code{output[[id]]})
    #' @returns A character vector of identifiers
    get_id = function(id, with_namespace = FALSE) {
      id <- sprintf("%s__%s", private$.name, id)
      if (with_namespace) {
        id <- self$`@ns`(id)
      }
      id
    },

    #' @description Register the function that renders an input
    #' @param input_name input name, a single string; the collected value
    #' uses this name
    #' @param ui_func \code{function(inputId, ns, restored_inputs)} returning
    #' the input element, or \code{NULL} to remove the input. \code{inputId}
    #' is the identifier without the \pkg{shiny} namespace (the same as
    #' \code{get_id(input_name)}), and \code{ns} is the namespace function
    #' \code{@ns}: give the element the identifier \code{ns(inputId)}, and use
    #' \code{inputId} as is wherever the namespace is added separately
    #' @returns The analysis object itself, invisibly
    set_input_ui = function(input_name, ui_func) {
      if (
        !is.character(input_name) ||
          length(input_name) != 1L ||
          is.na(input_name) ||
          !nzchar(input_name)
      ) {
        stop("`input_name` must be a single non-empty string", call. = FALSE)
      }
      private$.ui[[input_name]] <- check_function_args(
        ui_func,
        name = sprintf("`ui_func` for input '%s'", input_name),
        allow_null = TRUE,
        arg_names = c("inputId", "ns", "restored_inputs")
      )
      invisible(self)
    },

    #' @description Render an input registered by \code{set_input_ui}
    #' @param input_name input name
    #' @param pipeline a \code{\link{PipelineTools}} instance from which the
    #' saved input values are restored, see
    #' \code{@collect_inputs_from_pipeline}
    #' @returns The value returned by the input function, which receives the
    #' identifier without namespace, the namespace function \code{@ns}, and
    #' the restored input values; \code{NULL} invisibly if the input is not
    #' registered
    `@render_input` = function(input_name, pipeline) {
      check_is_pipeline(pipeline)
      ui_func <- private$.ui[[input_name]]
      if (!is.function(ui_func)) {
        return(invisible())
      }
      ui_func(
        inputId = self$get_id(input_name, with_namespace = FALSE),
        ns = self$`@ns`,
        restored_inputs = self$`@collect_inputs_from_pipeline`(pipeline)
      )
    },

    #' @description Render all registered inputs, each showing the input
    #' values saved in the pipeline; requires \pkg{htmltools}
    #' @param pipeline a \code{\link{PipelineTools}} instance from which the
    #' saved input values are restored
    #' @returns An \pkg{htmltools} tag list of the rendered inputs
    render_inputs = function(pipeline) {
      stopifnot(package_installed("htmltools"))
      htmltools <- asNamespace("htmltools")
      htmltools$tagList(unname(lapply(self$input_names, function(name) {
        self$`@render_input`(input_name = name, pipeline = pipeline)
      })))
    },

    #' @description Register the function that collects the input values
    #' from \pkg{shiny}
    #' @param collect_func \code{function(session)} returning the input
    #' values as a named list, where \code{session} is scoped to the analysis
    #' \code{namespace}; \code{NULL} restores the default, which reads the
    #' registered inputs within \code{shiny::isolate()}. A custom function is
    #' called as is, so it should isolate its own reads if it may run outside
    #' of a reactive context
    #' @returns The analysis object itself, invisibly
    set_collect_inputs_from_shiny = function(collect_func) {
      private$.collect_from_shiny <- check_function_args(
        collect_func,
        name = "collect_func",
        allow_null = TRUE,
        arg_names = "session"
      )
      invisible(self)
    },

    #' @description Collect the input values from a \pkg{shiny} session
    #' @param session \pkg{shiny} session; any scope works, since the values
    #' are read under the analysis \code{namespace}
    #' @returns A named list of input values; by default one per registered
    #' input, which is \code{NULL} if the session does not have it
    `@collect_inputs_from_shiny` = function(session) {
      session <- get_shiny_root_session(session)$makeScope(private$.namespace)
      collect_func <- private$.collect_from_shiny
      if (is.function(collect_func)) {
        inputs <- collect_func(session = session)
      } else {
        shiny <- asNamespace("shiny")
        input_names <- self$input_names
        inputs <- shiny$isolate({
          structure(
            names = input_names,
            lapply(input_names, function(input_name) {
              session$input[[self$get_id(input_name)]]
            })
          )
        })
      }
      inputs
    },

    #' @description Register the function that restores the input values
    #' from a pipeline
    #' @param collect_func \code{function(pipeline_settings)} returning the
    #' input values as a named list, where \code{pipeline_settings} is the
    #' named list of pipeline settings; \code{NULL} restores the default,
    #' which reads the settings named \code{inputs_settings_name}. The
    #' settings are resolved in the dashboard and when the pipeline is
    #' compiled, but come straight from the settings file when the pipeline
    #' runs, so settings stored as external data differ between the two
    #' @returns The analysis object itself, invisibly
    set_collect_inputs_from_pipeline = function(collect_func) {
      # UPDATE: use pipeline_settings so we do not need to expose pipelines here
      # Any dynamical pipeline targets should be obtained during preprocess
      private$.collect_from_pipeline <- check_function_args(
        collect_func,
        name = "collect_func",
        allow_null = TRUE,
        arg_names = "pipeline_settings"
      )
      invisible(self)
    },

    #' @description Restore the input values saved in a pipeline
    #' @param pipeline_settings named list of pipeline settings, or a
    #' \code{\link{PipelineTools}} instance whose settings are used
    #' @returns A named list of input values; by default the pipeline
    #' settings named \code{inputs_settings_name}, or an empty list if no
    #' values have been saved
    `@collect_inputs_from_pipeline` = function(pipeline_settings) {
      if (inherits(pipeline_settings, "PipelineTools")) {
        pipeline_settings <- pipeline_settings$get_settings()
      }
      collect_func <- private$.collect_from_pipeline
      if (is.function(collect_func)) {
        inputs <- collect_func(pipeline_settings = pipeline_settings)
      } else {
        inputs <- as.list(pipeline_settings[[self$inputs_settings_name]])
      }
      inputs
    },

    #' @description Register the function that converts the input values
    #' before they are saved to a pipeline
    #' @param store_func \code{function(inputs, pipeline)} returning the
    #' named list to save as the pipeline settings named
    #' \code{inputs_settings_name}; its value is always saved.
    #' \code{NULL} restores the default, which saves the input values
    #' unchanged. This is the only step that receives the pipeline, so it may
    #' save some values as other settings; those are not part of the saved
    #' inputs, so list them in the \code{pipeline_targets} of
    #' \code{set_preprocess} if the analysis depends on them
    #' @returns The analysis object itself, invisibly
    set_store_inputs_to_pipeline = function(store_func) {
      private$.store_to_pipeline <- check_function_args(
        store_func,
        name = "store_func",
        allow_null = TRUE,
        # This is the rare places where pipeline object will be directly exposed to analyzer
        arg_names = c("inputs", "pipeline")
      )
      invisible(self)
    },

    #' @description Save the input values to the pipeline settings named
    #' \code{inputs_settings_name}
    #' @param inputs input values, usually from
    #' \code{@collect_inputs_from_shiny}
    #' @param pipeline a \code{\link{PipelineTools}} instance
    #' @returns The saved value, which is the value returned by the store
    #' function (the input values by default), invisibly
    `@store_inputs_to_pipeline` = function(inputs, pipeline) {
      check_is_pipeline(pipeline)
      store_func <- private$.store_to_pipeline
      if (is.function(store_func)) {
        inputs <- store_func(inputs = inputs, pipeline = pipeline)
      }
      pipeline$set_settings(
        .list = structure(
          list(inputs),
          names = self$inputs_settings_name
        )
      )
      invisible(inputs)
    },

    #' @description Check that the input values survive being saved to a
    #' settings file and read back; the check uses a temporary copy of the
    #' pipeline, so \code{pipeline} is not changed
    #' @param pipeline a \code{\link{PipelineTools}} instance holding the
    #' input values to check
    #' @returns \code{TRUE} if the values read back are identical to the
    #' original ones, otherwise \code{FALSE}
    `@test_roundtrip_pipeline_inputs` = function(pipeline) {
      tf <- tempfile()
      on.exit(
        {
          if (file.exists(tf)) {
            unlink(tf, recursive = TRUE)
          }
        },
        add = TRUE
      )
      # Create another pipeline with empty settings
      temporary_pipeline <- pipeline$fork(path = tf, temporary = TRUE)
      unlink(temporary_pipeline$settings_path)
      file.create(temporary_pipeline$settings_path)

      # Reload temporary pipeline
      temporary_pipeline <- pipeline_from_path(temporary_pipeline$pipeline_path)

      # the temporary pipeline should get empty settings
      # temporary_pipeline$get_settings()

      # Get analysis inputs
      inputs <- self$`@collect_inputs_from_pipeline`(
        pipeline_settings = pipeline$get_settings()
      )

      # forward trip: store inputs to the temporary pipeline
      self$`@store_inputs_to_pipeline`(
        inputs = inputs,
        pipeline = temporary_pipeline
      )

      # Reload settings and remove cache; without `dry_run = FALSE`, the
      # settings are only read, not applied
      temporary_pipeline$import_settings(
        temporary_pipeline$settings_path,
        dry_run = FALSE
      )

      # backward trip: restore inputs from temporary settings
      inputs2 <- self$`@collect_inputs_from_pipeline`(
        pipeline_settings = temporary_pipeline$get_settings()
      )

      # Roundtrip should give identical inputs
      identical(inputs, inputs2)
    },

    #' @description Register the \pkg{shiny} module server
    #' @param server_func \code{function(input, output, session)}, or
    #' \code{NULL} to remove the server
    #' @returns The analysis object itself, invisibly
    set_shiny_server = function(server_func) {
      private$.server <- check_function_args(
        server_func,
        name = "server_func",
        allow_null = TRUE,
        arg_names = c("input", "output", "session")
      )
      invisible(self)
    },

    #' @description Start the \pkg{shiny} module server registered by
    #' \code{set_shiny_server}
    #' @param session \pkg{shiny} session; any scope works, since the server
    #' always runs under the analysis \code{namespace}
    #' @returns The value returned by the server function; \code{NULL}
    #' invisibly if no server is registered
    `@shiny_server` = function(session) {
      root_session <- get_shiny_root_session(session)
      if (!is.function(private$.server)) {
        return(invisible())
      }
      asNamespace("shiny")$moduleServer(
        id = private$.namespace,
        module = private$.server,
        session = root_session
      )
    },

    #' @description Register the \code{preprocess} step
    #' @param preprocess_func \code{function(value, pipeline_targets)}
    #' returning the processed values, which must hold everything the
    #' analyze step needs, or \code{NULL} to remove the step
    #' @param pipeline_targets names of the pipeline targets that must be
    #' built before the \code{preprocess} step; their values are passed to
    #' the \code{preprocess} step only. Default is \code{NULL} (none)
    #' @returns The analysis object itself, invisibly
    set_preprocess = function(preprocess_func, pipeline_targets = NULL) {
      if (
        !is.null(pipeline_targets) &&
          (!is.character(pipeline_targets) ||
            anyNA(pipeline_targets) ||
            !all(nzchar(pipeline_targets)) ||
            anyDuplicated(pipeline_targets) > 0)
      ) {
        stop(
          "`pipeline_targets` must be `NULL` or a character vector of unique non-empty target names",
          call. = FALSE
        ) # nolint: line_length_linter.
      }
      preprocess_func <- check_function_args(
        preprocess_func,
        name = "preprocess_func",
        allow_null = TRUE,
        arg_names = c("value", "pipeline_targets")
      )
      private$.preprocess <- preprocess_func
      private$.pipeline_targets <- as.character(pipeline_targets)
      invisible(self)
    },

    #' @description Process the collected input values before the analysis
    #' @param value input values, usually from
    #' \code{@collect_inputs_from_pipeline}
    #' @param pipeline_targets named list of pipeline target values, which
    #' must include every target in the \code{pipeline_targets} field, for
    #' example \code{pipeline[analysis$pipeline_targets, simplify = FALSE]}
    #' @returns The processed values, or \code{value} if no
    #' \code{preprocess} step is registered
    `@preprocess_data` = function(value, pipeline_targets = list()) {
      pipeline_targets <- private$.pick_pipeline_targets(pipeline_targets)
      if (!is.function(private$.preprocess)) {
        return(value)
      }
      # Using do.call to avoid including calls in the error messages
      do.call(
        private$.preprocess,
        list(value = value, pipeline_targets = pipeline_targets)
      )
    },

    #' @description Register the analyze step
    #' @param analyze_func \code{function(value, options)} returning the
    #' analysis result, or \code{NULL} to remove the step
    #' @returns The analysis object itself, invisibly
    set_analyze = function(analyze_func) {
      private$.analyze <- check_function_args(
        analyze_func,
        name = "analyze_func",
        allow_null = TRUE,
        arg_names = c("value", "options")
      )
      invisible(self)
    },

    #' @description Run the analysis with the current \code{options}; this
    #' method does not call \code{@preprocess_data}, so pass its result in.
    #' The analyze step receives only the processed values and the options
    #' @param value_processed processed values, usually returned by
    #' \code{@preprocess_data}
    #' @returns An object of class \verb{RAVEPipelineAnalysis_results}: a list
    #' with the analysis name (\code{analysis_name}) and the analysis result
    #' (\code{results}), which is \code{value_processed} if no analyze step is
    #' registered
    `@analyze_data` = function(value_processed) {
      if (is.function(private$.analyze)) {
        results <- do.call(
          private$.analyze,
          list(
            value = value_processed,
            options = self$options
          )
        )
      } else {
        results <- value_processed
      }
      structure(
        class = "RAVEPipelineAnalysis_results",
        list(
          analysis_name = private$.name,
          results = results
        )
      )
    },

    #' @description Register the visualize step
    #' @param visualize_func \code{function(value, options)} that prints,
    #' plots, or writes text, or \code{NULL} to remove the step
    #' @returns The analysis object itself, invisibly
    set_visualize = function(visualize_func) {
      private$.visualize <- check_function_args(
        visualize_func,
        name = "visualize_func",
        allow_null = TRUE,
        arg_names = c("value", "options")
      )
      invisible(self)
    },

    #' @description Visualize the analysis result with the current
    #' \code{options}
    #' @param value analysis result from \code{@analyze_data}, whose
    #' \code{results} are visualized; it must come from this analysis. A plain
    #' value that is not such a result is visualized as is
    #' @returns The value returned by the visualize function, visible or
    #' invisible as that function returned it (so a returned plot object is
    #' printed at top level or in a report chunk); \code{NULL} invisibly if
    #' no visualize step is registered
    `@visualize_data` = function(value) {
      if (!is.function(private$.visualize)) {
        return(invisible())
      }
      if (inherits(value, "RAVEPipelineAnalysis_results")) {
        analysis_name <- paste(
          .subset2(value, "analysis_name") %||% "[Unknown]",
          collapse = ""
        )
        if (!identical(private$.name, analysis_name)) {
          stop(
            "Inconsistent analysis for visualization: expected analysis: `",
            private$.name,
            "`, not `",
            analysis_name,
            "`."
          )
        }
        value <- .subset2(value, "results")
      }

      do.call(private$.visualize, list(value = value, options = self$options))
    },

    #' @description Run the analysis with a pipeline, without the 'RAVE'
    #' dashboard
    #' @param pipeline a \code{\link{PipelineTools}} instance
    #' @param step where to stop: \code{"inputs"} returns the collected input
    #' values, \code{"preprocess"} the processed values, and \code{"analyze"}
    #' the analysis result; \code{"all"} and \code{"visualize"} are the same,
    #' and also run the visualize step
    #' @param eval_method \code{"run"} saves the input values to the pipeline
    #' and builds the target \code{results_target_name}, which must exist;
    #' \code{"debug"} computes the analysis in this session without that
    #' target, for analyses not yet compiled into the pipeline, and does not
    #' save the input values. In debug mode, and for \code{step} set to
    #' \code{"preprocess"}, the prerequisite targets are read from the
    #' pipeline, so they must have been built
    #' @param session \pkg{shiny} session to collect the input values from;
    #' default is \code{NULL}, which restores them from the pipeline settings
    #' @param visualization_method \code{"direct"} calls the visualize step;
    #' \code{"html"} renders it as an \verb{HTML} fragment, which requires
    #' \pkg{rmarkdown}
    #' @param ... passed to the \code{run} method of the pipeline when
    #' \code{eval_method} is \code{"run"}
    #' @returns Depends on \code{step}: the input values, the processed values,
    #' the analysis result, or the value returned by the visualize step
    #' (an \pkg{htmltools} \verb{HTML} fragment if \code{visualization_method}
    #' is \code{"html"})
    run = function(
      pipeline,
      step = c("all", "inputs", "preprocess", "analyze", "visualize"),
      eval_method = c("run", "debug"),
      session = NULL,
      visualization_method = c("direct", "html"),
      ...
    ) {
      # For running entire analysis from pipeline without shiny
      # eval means do not rely on the pipeline target to build, for debugging
      # when analysis is not yet baked into pipeline as targets

      step <- match.arg(step)
      eval_method <- match.arg(eval_method)
      visualization_method <- match.arg(visualization_method)

      
      if (eval_method == "run" && !isTRUE(self$results_target_name %in% pipeline$target_table$Names)) {
        stop("The analysis does not have build target in the pipeline. If you are debugging the module, use `eval_method='debug'`.")
      }

      # assumes the analysis is built into the pipeline
      if (!is.null(session)) {
        cleaned_inputs <- self$`@collect_inputs_from_shiny`(session)
      } else {
        cleaned_inputs <- self$`@collect_inputs_from_pipeline`(
          pipeline_settings = pipeline$get_settings()
        )
      }

      # This step saves inputs to pipeline
      if (eval_method == "run") {
        self$`@store_inputs_to_pipeline`(inputs = cleaned_inputs, pipeline = pipeline)
      }

      if (step == "inputs") {
        return(cleaned_inputs)
      }

      if (step == "preprocess") {
        # preprocess
        preprocessed <- self$`@preprocess_data`(
          value = cleaned_inputs,
          pipeline_targets = pipeline[self$pipeline_targets, simplify = FALSE]
        )
        return(preprocessed)
      }

      # analyze
      if (eval_method == "run") {
        analysis_results <- pipeline$run(names = self$results_target_name, ...)
      } else {
        preprocessed <- self$`@preprocess_data`(
          value = cleaned_inputs,
          pipeline_targets = pipeline[self$pipeline_targets, simplify = FALSE]
        )
        analysis_results <- self$`@analyze_data`(value_processed = preprocessed)
      }
      
      re <- .subset2(analysis_results, "results")
      if (step == "analyze") {
        return(re)
      }

      # visualize
      switch(
        visualization_method,
        "direct" = {
          self$`@visualize_data`(value = analysis_results)
        },
        "html" = {
          render_as_html_fragment(analysis = self, results = analysis_results)
        }
      )
    },

    #' @description Save the input values to the pipeline, then build the
    #' target \code{results_target_name} as a \pkg{shiny} extended task
    #' @param pipeline a \code{\link{PipelineTools}} instance
    #' @param session \pkg{shiny} session to collect the input values from;
    #' default is \code{NULL}, which restores them from the pipeline settings
    #' @param ... passed to the \code{run_as_task} method of the pipeline
    #' @returns The task returned by the \code{run_as_task} method of the
    #' pipeline
    run_as_task = function(pipeline, session = NULL, ...) {
      if (!is.null(session)) {
        inputs <- self$`@collect_inputs_from_shiny`(session)
      } else {
        inputs <- self$`@collect_inputs_from_pipeline`(
          pipeline_settings = pipeline$get_settings()
        )
      }
      self$`@store_inputs_to_pipeline`(inputs = inputs, pipeline = pipeline)

      name <- self$results_target_name
      pipeline$run_as_task(names = name, ...)
    },

    #' @description Create the pipeline target specifications for this
    #' analysis; called when the pipeline is compiled
    #' @param varname name of the variable that holds this analysis in the
    #' pipeline environment; the generated code refers to it
    #' @param format,cue storage format and \code{targets} cue of the
    #' results target
    #' @returns A list of target specifications: the cleaned-inputs target
    #' (only with a custom pipeline collector), then the results target
    `@build_targets` = function(varname, format = NULL, cue = "thorough") {
      # varname <- "streamline_collision_detection_analyzer"
      # private <- self$.__enclos_env__$private

      targets <- list()
      #   generate_target <- function(
      # expr, export, format, deps = NULL,
      # cue = "thorough", pattern = NULL, quoted = TRUE)

      if (is.function(private$.collect_from_pipeline)) {
        # target 1: cleaned target, only needed when the analysis has custom function to collect inputs from pipeline
        # cleaned-input target is needed

        cleaned_target_name <- sprintf(
          "analysis_cleaned_inputs_%s",
          private$.name
        )
        cleaned_target_expr <- sprintf(
          "%s <- %s[[\"@collect_inputs_from_pipeline\"]](settings)",
          cleaned_target_name,
          varname
        )

        # will eventually goes to rave_knitr_build -> rave_knit_r, but some extras are needed
        targets[[length(targets) + 1]] <- list(
          language = "R",
          label = sprintf("__Collect_analysis_inputs-%s", private$.name),
          export = cleaned_target_name,
          code = cleaned_target_expr,
          deps = c(self$inputs_settings_name, "settings"),
          cue = "thorough",
          format = NULL
        )
      } else {
        # self$inputs_settings_name -> this target is built by pipeline
        cleaned_target_name <- self$inputs_settings_name
      }

      # target 2: analysis part: preprocess + analysis
      dep_names <- as.character(self$pipeline_targets)
      analysis_target_expr <- paste(
        collapse = "\n",
        c(
          sprintf("%s <- local({", self$results_target_name),
          sprintf("  self <- %s", varname),
          sprintf("  cleaned_inputs <- %s", cleaned_target_name),
          sprintf(
            "  dep_vars <- list(%s)",
            paste(sprintf("%s = %s", dep_names, dep_names), collapse = ", ")
          ),
          "  value_processed <- self$`@preprocess_data`(value = cleaned_inputs, pipeline_targets = dep_vars)",
          "  self$`@analyze_data`(value_processed = value_processed)",
          "})"
        )
      )

      targets[[length(targets) + 1]] <- list(
        language = "R",
        label = sprintf("__Build_analysis_result-%s", private$.name),
        export = self$results_target_name,
        code = analysis_target_expr,
        deps = c(cleaned_target_name, dep_names),
        cue = cue,
        format = format,
        is_delayed = TRUE
      )

      targets
    }
  ),
  active = list(
    #' @field name analysis name, read-only
    name = function(v) {
      if (!missing(v)) {
        stop(
          "`name` is read-only; create a new analysis instead",
          call. = FALSE
        )
      }
      private$.name
    },

    #' @field input_names names of the registered inputs, read-only
    input_names = function() {
      as.character(names(private$.ui))
    },

    #' @field options named list of options passed to the analyze and
    #' visualize steps; assigning replaces the whole list and must be a named
    #' list (\code{list()} clears it). Use \code{set_option} to change
    #' individual options
    options = function(v) {
      if (!missing(v)) {
        if (!is_named_list(v)) {
          stop("`options` must be a named list", call. = FALSE)
        }
        private$.options <- v
      }
      private$.options
    },

    #' @field pipeline_targets names of the pipeline targets whose values the
    #' \code{preprocess} step receives, read-only; see \code{set_preprocess}
    pipeline_targets = function() {
      private$.pipeline_targets
    },

    #' @field inputs_settings_name name of the pipeline settings that holds
    #' the saved input values, read-only
    inputs_settings_name = function() {
      sprintf("analysis_inputs_%s", private$.name)
    },

    #' @field results_target_name name of the pipeline target that holds the
    #' analysis result, read-only
    results_target_name = function() {
      sprintf("analysis_results_%s", private$.name)
    },

    #' @field @ns \pkg{shiny} namespace function: \code{ns(id)} adds the
    #' namespace prefix to \code{id}, and \code{ns(NULL)} returns the prefix
    `@ns` = function() {
      if (system.file(package = "shiny") != "") {
        asNamespace("shiny")$NS(private$.namespace)
      } else {
        ns_prefix <- private$.namespace
        function(id) {
          if (length(id) == 0) {
            return(ns_prefix)
          }
          paste(ns_prefix, id, sep = "-")
        }
      }
    }
  )
)

# Renders the visualize step of `analysis` for `results`, the value returned
# by `analysis$`@analyze_data`()`, as an HTML fragment
render_as_html_fragment <- function(analysis, results) {
  if (!package_installed("rmarkdown") || !package_installed("htmltools")) {
    stop(
      "Rendering the visualization as HTML requires the suggested packages `rmarkdown` and `htmltools`.",
      call. = FALSE
    )
  }
  render_env <- new.env(parent = globalenv())
  render_env$analysis <- analysis
  render_env$results <- results

  # Create a template; an R Markdown one, since roxygen reads any line that
  # starts with #' as documentation, even inside a string
  frag_template_str <- r"(---
title: "`r analysis$description`"
description: "Generated by RAVE"
date: "`r date()`"
---

```{r setup, include=FALSE, message=FALSE, echo=FALSE, results='hide'}
knitr::opts_knit$set(upload.fun = knitr::image_uri, out.width = "100%")
```

```{r, echo=FALSE, eval=TRUE}
analysis$`@visualize_data`(results)
```
)"
  tf_rmd <- tempfile(fileext = ".Rmd")
  tf_html <- tempfile(fileext = ".html")

  on.exit(
    {
      unlink(tf_rmd)
      unlink(tf_html)
    },
    add = TRUE
  )

  writeLines(frag_template_str, tf_rmd)

  rmarkdown <- asNamespace("rmarkdown")

  rmarkdown$render(
    input = tf_rmd,
    output_file = tf_html,
    output_format = rmarkdown$html_fragment(),
    envir = render_env,
    quiet = TRUE
  )

  html_str <- readLines(tf_html)
  asNamespace("htmltools")$HTML(paste(html_str, collapse = "\n"))
}