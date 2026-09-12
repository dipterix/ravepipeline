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

# Whether `x` is atomic data: `NULL`, an atomic vector, or a list (such as a
# data frame) whose elements are all atomic data; environments (including R6
# objects) and functions are not
is_atomic_data <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  for (item in x) {
    if (!is_atomic_data(item)) {
      return(FALSE)
    }
  }
  TRUE
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
#' the prerequisite pipeline targets to the methods that need them.
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

    # The values of the declared pipeline targets, from the named list
    # `pipeline_targets`
    .pick_pipeline_targets = function(pipeline_targets) {
      target_names <- private$.pipeline_targets
      missing_names <- target_names[!target_names %in% names(pipeline_targets)]
      if (length(missing_names)) {
        stop(sprintf(
          "Analysis '%s' requires the values of pipeline targets %s. The following targets are missing: %s",
          private$.name, paste(sprintf("`%s`", target_names), collapse = ", "),
          paste(sprintf("`%s`", missing_names), collapse = ", ")
        ), call. = FALSE)
      }
      as.list(pipeline_targets)[target_names]
    }
  ),
  public = list(

    #' @field options named list of options passed to the analyze and
    #' visualize steps; assigning replaces the whole list, and \code{NULL}
    #' clears it
    options = structure(list(), names = character(0L)),

    set_option = function(..., .list = list(), .clear_first = FALSE) {
      if (.clear_first) {
        self$options <- structure(list(), names = character(0L))
      }
      self$options <- utils::modifyList(self$options, c(list(...), .list))
      invisible(self)
    },

    #' @description Constructor
    #' @param name analysis name, a single string of letters, digits, and
    #' underscores that starts with a letter; used as the prefix of the
    #' input identifiers
    #' @param namespace \pkg{shiny} module namespace under which the inputs
    #' are rendered, usually the module ID; a single non-empty string
    initialize = function(name, namespace) {
      if (!is.character(name) || length(name) != 1L ||
          !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", name)) {
        stop("`name` must be a single string of letters, digits, and underscores, starting with a letter", call. = FALSE) # nolint: line_length_linter.
      }
      if (!is.character(namespace) || length(namespace) != 1L ||
          is.na(namespace) || !nzchar(namespace)) {
        stop("`namespace` must be a single non-empty string", call. = FALSE)
      }
      private$.name <- name
      private$.namespace <- namespace
      private$.ui <- list()
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
    #' @param ui_func \code{function(inputId, restored_inputs)} returning the
    #' input element, or \code{NULL} to remove the input
    #' @returns The analysis object itself, invisibly
    set_input_ui = function(input_name, ui_func) {
      if (!is.character(input_name) || length(input_name) != 1L ||
          is.na(input_name) || !nzchar(input_name)) {
        stop("`input_name` must be a single non-empty string", call. = FALSE)
      }
      private$.ui[[input_name]] <- check_function_args(
        ui_func, name = sprintf("`ui_func` for input '%s'", input_name),
        allow_null = TRUE, arg_names = c("inputId", "restored_inputs")
      )
      invisible(self)
    },

    #' @description Render an input registered by \code{set_input_ui}
    #' @param input_name input name
    #' @param pipeline a \code{\link{PipelineTools}} instance from which the
    #' saved input values are restored, see
    #' \code{collect_inputs_from_pipeline}
    #' @returns The value returned by the input function, which receives the
    #' identifier with namespace and the restored input values; \code{NULL}
    #' invisibly if the input is not registered
    `@render_input` = function(input_name, pipeline) {
      check_is_pipeline(pipeline)
      ui_func <- private$.ui[[input_name]]
      if (!is.function(ui_func)) {
        return(invisible())
      }
      ui_func(
        inputId = self$get_id(input_name, with_namespace = TRUE),
        restored_inputs = self$`@collect_inputs_from_pipeline`(pipeline)
      )
    },

    #' @description Register the function that collects the input values
    #' from \pkg{shiny}
    #' @param collect_func \code{function(session)} returning the input
    #' values as a named list, where \code{session} is scoped to the analysis
    #' \code{namespace}; \code{NULL} restores the default, which reads the
    #' registered inputs
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
    #' @param collect_func \code{function(pipeline)} returning the input
    #' values as a named list; \code{NULL} restores the default, which reads
    #' the pipeline settings named \code{inputs_settings_name}
    #' @returns The analysis object itself, invisibly
    set_collect_inputs_from_pipeline = function(collect_func) {
      private$.collect_from_pipeline <- check_function_args(
        collect_func,
        name = "collect_func",
        allow_null = TRUE,
        arg_names = "pipeline"
      )
      invisible(self)
    },

    #' @description Restore the input values saved in a pipeline
    #' @param pipeline a \code{\link{PipelineTools}} instance
    #' @returns A named list of input values; by default the pipeline
    #' settings named \code{inputs_settings_name}, or an empty list if no
    #' values have been saved
    `@collect_inputs_from_pipeline` = function(pipeline) {
      check_is_pipeline(pipeline)
      collect_func <- private$.collect_from_pipeline
      if (is.function(collect_func)) {
        inputs <- collect_func(pipeline = pipeline)
      } else {
        inputs <- pipeline$get_settings(self$inputs_settings_name, default = list())
      }
      inputs
    },

    #' @description Register the function that converts the input values
    #' before they are saved to a pipeline
    #' @param store_func \code{function(inputs)} returning the named list to
    #' save; \code{NULL} restores the default, which saves the input values
    #' unchanged
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
    #' \code{collect_inputs_from_shiny}
    #' @param pipeline a \code{\link{PipelineTools}} instance
    #' @returns The saved values, invisibly
    `@store_inputs_to_pipeline` = function(inputs, pipeline) {
      check_is_pipeline(pipeline)
      store_func <- private$.store_to_pipeline
      if (is.function(store_func)) {
        inputs <- store_func(inputs = inputs, pipeline = pipeline)
      }
      if (!is_key_missing(inputs)) {
        pipeline$set_settings(.list = structure(
          list(inputs),
          names = self$inputs_settings_name
        ))
      }
      invisible(inputs)
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
    #' returning the processed values, or \code{NULL} to remove the step
    #' @param pipeline_targets names of the pipeline targets that must be
    #' built before the \code{preprocess} step; their values are passed to
    #' the \code{preprocess} and analyze steps. Default is \code{NULL}
    #' (none)
    #' @returns The analysis object itself, invisibly
    set_preprocess = function(preprocess_func, pipeline_targets = NULL) {
      if (!is.null(pipeline_targets) && (
        !is.character(pipeline_targets) || anyNA(pipeline_targets) ||
        !all(nzchar(pipeline_targets)) || anyDuplicated(pipeline_targets) > 0
      )) {
        stop("`pipeline_targets` must be `NULL` or a character vector of unique non-empty target names", call. = FALSE) # nolint: line_length_linter.
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
    #' \code{collect_inputs_from_pipeline}
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
      private$.preprocess(value = value, pipeline_targets = pipeline_targets)
    },

    #' @description Register the analyze step
    #' @param analyze_func \code{function(value, pipeline_targets, options)}
    #' returning the analysis result, or \code{NULL} to remove the step
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
    #' method does not call \code{preprocess_data}, so pass its result in
    #' @param value_processed processed values, usually returned by
    #' \code{preprocess_data}
    #' @param pipeline_targets named list of pipeline target values, as in
    #' \code{preprocess_data}
    #' @returns The analysis result, or \code{value_processed} if no analyze
    #' step is registered
    `@analyze_data` = function(value_processed) {
      if (!is.function(private$.analyze)) {
        return(value_processed)
      }
      private$.analyze(
        value = value_processed,
        options = self$options
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
    #' @param value analysis result, usually from \code{analyze_data}
    #' @returns The value returned by the visualize function, visible or
    #' invisible as that function returned it (so a returned plot object is
    #' printed at top level or in a report chunk); \code{NULL} invisibly if
    #' no visualize step is registered
    `@visualize_data` = function(value) {
      if (!is.function(private$.visualize)) {
        return(invisible())
      }
      private$.visualize(value = value, options = self$options)
    }

  ),
  active = list(

    #' @field name analysis name, read-only
    name = function(v) {
      if (!missing(v)) {
        stop("`name` is read-only; create a new analysis instead", call. = FALSE)
      }
      private$.name
    },

    #' @field input_names names of the registered inputs, read-only
    input_names = function() {
      as.character(names(private$.ui))
    },

    #' @field pipeline_targets names of the pipeline targets whose values the
    #' \code{preprocess} and analyze steps receive, read-only; see
    #' \code{set_preprocess}
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

    #' @field ns \pkg{shiny} namespace function: \code{ns(id)} adds the
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

