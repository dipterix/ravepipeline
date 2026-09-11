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

#' Modular analysis unit for 'RAVE' pipelines
#'
#' @description
#' A light-weight container that lets module developers describe one analysis
#' as a handful of plain functions, while the 'RAVE' dashboard (the
#' controller) decides when to render the inputs, collect their values, run
#' the analysis, and show the results. Every step is optional, and none of
#' them requires \pkg{shiny}.
#'
#' @details
#' A controller runs an analysis in the following order; without registered
#' functions, \code{preprocess_data} and \code{analyze_data} return their
#' input unchanged, and \code{visualize_data} does nothing:
#' \preformatted{
#' value  <- analysis$collect_inputs(input)
#' value  <- analysis$preprocess_data(value)
#' result <- analysis$analyze_data(value)
#' analysis$visualize_data(result)
#' }
#'
#' Developers register the steps with the \code{set_*} methods. Each step
#' function must accept the arguments listed below, by name; a function with a
#' \code{...} formal argument accepts all of them.
#' \describe{
#' \item{\code{set_input_ui}}{\code{function(inputId, pipeline)};
#' returns whatever the dashboard renders, usually a \pkg{shiny} input whose
#' identifier must be \code{inputId}}
#' \item{\code{set_shiny_server}}{\code{function(input, output, session)};
#' a \pkg{shiny} module server running in the analysis \code{namespace}}
#' \item{\code{set_preprocess}}{\code{function(value, pipeline)};
#' converts the collected input values into analysis parameters, and should
#' call \code{stop()} to reject invalid values before the analysis runs}
#' \item{\code{set_analyze}}{\code{function(value, pipeline, options)};
#' runs synchronously on the output of \code{preprocess_data}, typically
#' saving it with \code{pipeline$set_settings()} before running or reading
#' pipeline targets, and returns the analysis result}
#' \item{\code{set_visualize}}{\code{function(value, pipeline, options)};
#' receives the analysis result and prints, plots, or writes text; the caller
#' (for example an R Markdown chunk) captures the output}
#' }
#'
#' Options are a named list shared by the analyze and visualize steps. Set the
#' defaults with \code{analysis$options <- list(...)}. Extra named arguments to
#' \code{analyze_data} update the options and are kept for later calls, while
#' extra arguments to \code{visualize_data} apply to that call only. An option
#' whose name partially matches the first argument of these methods (for
#' example \code{val}) must be passed through \code{.list}.
#'
#' Input identifiers are \code{"<name>__<input_name>"} (see \code{get_id}),
#' placed under the \pkg{shiny} module \code{namespace}, which defaults to the
#' pipeline name; standard 'RAVE' modules use the same name for the module and
#' its pipeline. Set \code{namespace} explicitly when they differ.
#'
#' @examples
#'
#' \dontrun{
#' 
#' # ---- A pipeline to work with ---------------------------------------
#' # Any 'RAVE' pipeline works; here is a bare template in a temporary folder
#' root_path <- tempfile()
#' pipeline_path <- pipeline_create_template(
#'   root_path = root_path, pipeline_name = "analysis_demo",
#'   overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
#' pipe <- pipeline_from_path(pipeline_path)
#'
#' # ---- Developer side: describe the analysis --------------------------
#' analysis <- RAVEPipelineAnalysis$new(name = "scatter", pipeline = pipe)
#'
#' # Inputs return whatever the dashboard renders, usually shiny inputs;
#' # plain HTML strings here so the example does not need shiny
#' analysis$set_input_ui("n", function(inputId, pipeline) {
#'   n <- pipeline$get_settings("n", default = 100)
#'   paste0('<input id="', inputId, '" type="number" value="', n, '">')
#' })
#' analysis$set_input_ui("col", function(inputId, pipeline) {
#'   col <- pipeline$get_settings("col", default = "steelblue")
#'   paste0('<input id="', inputId, '" value="', col, '">')
#' })
#'
#' # Pre-process: turn raw input values into analysis parameters
#' analysis$set_preprocess(function(value, pipeline) {
#'   value$n <- suppressWarnings(as.integer(value$n))
#'   if (is.na(value$n) || value$n < 2) {
#'     stop("`n` must be an integer greater than 1")
#'   }
#'   value
#' })
#'
#' # Analyze: save the parameters to the pipeline, then compute
#' analysis$set_analyze(function(value, pipeline, options) {
#'   pipeline$set_settings(.list = value)
#'   if (length(options$seed)) {
#'     set.seed(options$seed)
#'   }
#'   x <- stats::rnorm(value$n)
#'   list(x = x, y = x + stats::rnorm(value$n), col = value$col)
#' })
#'
#' # Visualize: any mix of printed text and plots
#' analysis$set_visualize(function(value, pipeline, options) {
#'   cat("Correlation:", round(stats::cor(value$x, value$y), 2), "\n")
#'   plot(value$x, value$y, col = value$col, pch = 16, main = options$main)
#' })
#'
#' # Default options
#' analysis$options <- list(main = "Simulated data")
#'
#' # ---- Controller side: what the dashboard does ----------------------
#' analysis$input_names
#' analysis$get_id("n")
#' analysis$get_id("n", with_namespace = TRUE)
#' analysis$render_input("n")
#'
#' # `input` mimics shiny's `input`: values keyed by input identifier
#' input <- list(scatter__n = "50", scatter__col = "orange")
#'
#' value <- analysis$collect_inputs(input)
#' value <- analysis$preprocess_data(value)
#'
#' # `seed` is kept in `analysis$options` for later calls
#' result <- analysis$analyze_data(value, seed = 42)
#' analysis$options
#'
#' # the analysis saved its parameters to the pipeline
#' pipe$get_settings("n")
#'
#' # `main` applies to this call only
#' analysis$visualize_data(result, main = "One-off title")
#' analysis$options$main
#'
#' # invalid input is rejected before the analysis runs
#' try(analysis$preprocess_data(list(n = "one", col = "red")))
#'
#' # ---- Clean up -------------------------------------------------------
#' unlink(root_path, recursive = TRUE)
#' 
#' }
#'
#' @export
RAVEPipelineAnalysis <- R6::R6Class(
  classname = "RAVEPipelineAnalysis",
  portable = TRUE,
  private = list(
    .name = character(0L),
    .ui = NULL,
    .server = NULL,
    .preprocess = NULL,
    .analyze = NULL,
    .visualize = NULL,
    .pipeline = NULL,
    .namespace = NULL,
    .options = NULL,

    # Updates the options keys in `opts` and keeps the others
    .merge_options = function(opts) {
      if (!length(opts)) {
        return(invisible())
      }
      if (!is_named_list(opts)) {
        stop("All options must be named", call. = FALSE)
      }
      new_options <- private$.options
      new_options[names(opts)] <- opts
      private$.options <- new_options
      invisible()
    }
  ),
  public = list(

    #' @description Constructor
    #' @param name analysis name, a single string of letters, digits, and
    #' underscores that starts with a letter; used as the prefix of the
    #' input identifiers
    #' @param pipeline a \code{\link{PipelineTools}} instance, see
    #' \code{\link{pipeline}}
    #' @param namespace \pkg{shiny} module namespace under which the inputs
    #' are rendered; default is the pipeline name
    initialize = function(name, pipeline, namespace = pipeline$pipeline_name) {
      if (!is.character(name) || length(name) != 1L ||
          !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", name)) {
        stop("`name` must be a single string of letters, digits, and underscores, starting with a letter", call. = FALSE) # nolint: line_length_linter.
      }
      private$.name <- name
      self$pipeline <- pipeline
      private$.namespace <- namespace
      private$.ui <- list()
      private$.options <- list()
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
        id <- self$ns(id)
      }
      id
    },

    #' @description Register the function that renders an input
    #' @param input_name input name, a single string; the collected value
    #' uses this name
    #' @param ui_func \code{function(inputId, pipeline)} returning the input
    #' element, or \code{NULL} to remove the input
    #' @returns The analysis object itself, invisibly
    set_input_ui = function(input_name, ui_func) {
      if (!is.character(input_name) || length(input_name) != 1L ||
          is.na(input_name) || !nzchar(input_name)) {
        stop("`input_name` must be a single non-empty string", call. = FALSE)
      }
      private$.ui[[input_name]] <- check_function_args(
        ui_func, name = sprintf("`ui_func` for input '%s'", input_name),
        allow_null = TRUE, arg_names = c("inputId", "pipeline")
      )
      invisible(self)
    },

    #' @description Render an input registered by \code{set_input_ui}
    #' @param input_name input name
    #' @returns The value returned by the input function, which receives the
    #' identifier with namespace; \code{NULL} invisibly if the input is not
    #' registered
    render_input = function(input_name) {
      ui_func <- private$.ui[[input_name]]
      if (!is.function(ui_func)) {
        return(invisible())
      }
      ui_func(
        inputId = self$get_id(input_name, with_namespace = TRUE),
        pipeline = self$pipeline
      )
    },

    #' @description Collect the values of all registered inputs
    #' @param input a \pkg{shiny} \code{input} object, or any list, whose
    #' elements are the input values keyed by identifier
    #' @param with_namespace whether the keys of \code{input} include the
    #' namespace; default is false, which matches the \code{input} inside
    #' the module server; set to true for the root session \code{input}
    #' @returns A named list of input values, one per registered input; a
    #' value is \code{NULL} if \code{input} does not contain it
    collect_inputs = function(input, with_namespace = FALSE) {
      input_names <- self$input_names
      structure(
        names = input_names,
        lapply(input_names, function(input_name) {
          input[[self$get_id(input_name, with_namespace = with_namespace)]]
        })
      )
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
    #' \code{set_shiny_server}; must run within a \pkg{shiny} session
    #' @param session \pkg{shiny} session; default is the current session.
    #' Any scope of the session works, since the server always runs under
    #' the analysis \code{namespace}
    #' @returns The value returned by the server function; \code{NULL}
    #' invisibly if no server is registered
    shiny_server = function(session = NULL) {
      if (!is.function(private$.server)) {
        return(invisible())
      }
      stopifnot(
        "Package `shiny` must be installed to run a shiny-server" = (system.file(package = "shiny") != "")
      )
      shiny <- asNamespace("shiny")
      if (is.null(session)) {
        session <- shiny$getDefaultReactiveDomain()
      }
      if (is.null(session)) {
        stop("`shiny_server()` must run within a shiny session, or with `session` specified", call. = FALSE) # nolint: line_length_linter.
      }
      root_session <- session$rootScope()
      namespace <- self$ns(NULL)
      if (!length(namespace)) {
        return(private$.server(
          input = root_session$input,
          output = root_session$output,
          session = root_session
        ))
      }
      shiny$moduleServer(id = namespace, module = private$.server, session = root_session)
    },

    #' @description Register the \code{preprocess} step
    #' @param preprocess_func \code{function(value, pipeline)} returning the
    #' processed values, or \code{NULL} to remove the step
    #' @returns The analysis object itself, invisibly
    set_preprocess = function(preprocess_func) {
      private$.preprocess <- check_function_args(
        preprocess_func,
        name = "preprocess_func",
        allow_null = TRUE,
        arg_names = c("value", "pipeline")
      )
      invisible(self)
    },

    #' @description Process the collected input values before the analysis
    #' @param value input values, usually from \code{collect_inputs}
    #' @returns The processed values, or \code{value} if no
    #' \code{preprocess} step is registered
    preprocess_data = function(value) {
      if (!is.function(private$.preprocess)) {
        return(value)
      }
      private$.preprocess(value = value, pipeline = self$pipeline)
    },

    #' @description Register the analyze step
    #' @param analyze_func \code{function(value, pipeline, options)}
    #' returning the analysis result, or \code{NULL} to remove the step
    #' @returns The analysis object itself, invisibly
    set_analyze = function(analyze_func) {
      private$.analyze <- check_function_args(
        analyze_func,
        name = "analyze_func",
        allow_null = TRUE,
        arg_names = c("value", "pipeline", "options")
      )
      invisible(self)
    },

    #' @description Run the analysis; this method does not call
    #' \code{preprocess_data}, so pass its result in
    #' @param value_processed processed values, usually returned by
    #' \code{preprocess_data}
    #' @param ...,.list named options to update and keep in \code{options}
    #' before the analysis runs; \code{.list} takes precedence over
    #' \code{...} for the same name
    #' @returns The analysis result, or \code{value_processed} if no analyze
    #' step is registered
    analyze_data = function(value_processed, ..., .list = list()) {
      private$.merge_options(c(list(...), as.list(.list)))
      if (!is.function(private$.analyze)) {
        return(value_processed)
      }
      private$.analyze(
        value = value_processed,
        pipeline = self$pipeline,
        options = self$options
      )
    },

    #' @description Register the visualize step
    #' @param visualize_func \code{function(value, pipeline, options)} that
    #' prints, plots, or writes text, or \code{NULL} to remove the step
    #' @returns The analysis object itself, invisibly
    set_visualize = function(visualize_func) {
      private$.visualize <- check_function_args(
        visualize_func,
        name = "visualize_func",
        allow_null = TRUE,
        arg_names = c("value", "pipeline", "options")
      )
      invisible(self)
    },

    #' @description Visualize the analysis result
    #' @param value analysis result, usually from \code{analyze_data}
    #' @param ...,.list named options for this call only; \code{options} is
    #' restored afterwards. \code{.list} takes precedence over \code{...}
    #' for the same name
    #' @returns The value returned by the visualize function, visible or
    #' invisible as that function returned it (so a returned plot object is
    #' printed at top level or in a report chunk); \code{NULL} invisibly if
    #' no visualize step is registered
    visualize_data = function(value, ..., .list = list()) {
      if (!is.function(private$.visualize)) {
        return(invisible())
      }
      options_orig <- private$.options
      on.exit({ private$.options <- options_orig }, add = TRUE)
      private$.merge_options(c(list(...), as.list(.list)))
      private$.visualize(
        value = value,
        pipeline = self$pipeline,
        options = self$options
      )
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

    #' @field options named list of options passed to the analyze and
    #' visualize steps; assigning replaces the whole list, and \code{NULL}
    #' clears it
    options = function(v) {
      if (!missing(v)) {
        if (is.null(v)) {
          v <- list()
        }
        if (!is_named_list(v)) {
          stop("`options` must be a named list", call. = FALSE)
        }
        private$.options <- v
      }
      private$.options
    },

    #' @field pipeline the \code{\link{PipelineTools}} instance that the
    #' analysis works with
    pipeline = function(v) {
      if (!missing(v)) {
        stopifnot("`pipeline` must be a RAVE pipeline [PipelineTools] object." = inherits(v, "PipelineTools"))
        private$.pipeline <- v
      }
      private$.pipeline
    },

    #' @field ns \pkg{shiny} namespace function: \code{ns(id)} adds the
    #' namespace prefix to \code{id}, and \code{ns(NULL)} returns the prefix
    ns = function() {
      if (system.file(package = "shiny") != "") {
        asNamespace("shiny")$NS(private$.namespace)
      } else {
        if (length(private$.namespace)) {
          ns_prefix <- paste(private$.namespace, collapse = "-")
        } else {
          ns_prefix <- character(0L)
        }

        function(id) {
          if (length(id) == 0) {
            return(ns_prefix)
          }
          if (length(ns_prefix) == 0) {
            return(id)
          }
          paste(ns_prefix, id, sep = "-")
        }
      }
    }

  )
)
