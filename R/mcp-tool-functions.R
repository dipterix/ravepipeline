# MCP Tool Implementation Functions
# These functions are called by MCP tools to interact with RAVE pipelines
# Each function is documented with roxygen2 tags that are used to generate
# the tool specifications in inst/mcp-tools.yaml

mcp_state <- local({
  env <- NULL
  function() {
    if(is.null(env)) {
      env <<- new.env(parent = emptyenv())
    }
    env
  }
})

#' List Available RAVE Pipelines
#'
#' @description List all installed RAVE pipelines available on the system.
#'
#' @return A list containing:
#' \describe{
#'   \item{pipelines}{Character vector, pipeline names}
#'   \item{count}{Integer, total number of available pipelines}
#' }
#'
#' @examples
#'
#' # MCP example response:
#' # {
#' #   "pipelines": [
#' #     "block_explorer", "custom_3d_viewer", "generate_surface_atlas",
#' #     "group_3d_viewer", "import_bids", "import_lfp_native",
#' #     "import_signals", "notch_filter", "power_clust",
#' #     "power_explorer", "project_overview", "reference_module",
#' #     "stimpulse_finder", "surface_reconstruction", "trace_viewer",
#' #     "voltage_clust", "wavelet_module", "yael_preprocess"
#' #   ],
#' #   "count": 18
#' # }
#'
#' \dontrun{
#' result <- mcp_list_rave_pipelines()
#' }
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_list_rave_pipelines <- function() {
  pipelines <- pipeline_list()
  list(
    pipelines = pipelines,
    count = length(pipelines)
  )
}

#' Load a RAVE Pipeline
#'
#' @description Load a RAVE pipeline by name. This makes the pipeline available
#' for configuration and execution. Creates a pipeline instance in the global
#' environment as 'pipe'.
#'
#' @param pipeline_name Character string, name of the pipeline to load.
#'   Use \code{mcp_list_rave_pipelines} to see available options.
#'   Example values: \code{"power_explorer"}, \code{"notch_filter"}, \code{"wavelet_module"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the pipeline was loaded successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{description}{Character, pipeline title/description from metadata}
#'   \item{targets}{Integer, number of computational targets in the pipeline}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # MCP example response:
#' # {
#' #   "success": true,
#' #   "message": "Loaded pipeline: power_explorer",
#' #   "description": "Explore Power or Amplitude of iEEG Spectrogram",
#' #   "targets": 56
#' # }
#'
#' \dontrun{
#' result <- mcp_load_rave_pipeline("power_explorer")
#' }
#'
#' @keywords mcp-tool mcp-category-setup
#' @noRd
mcp_load_rave_pipeline <- function(pipeline_name) {
  state <- mcp_state()

  tryCatch({
    state$pipe <- pipeline(pipeline_name)
    list(
      success = TRUE,
      message = sprintf("Loaded pipeline: %s", pipeline_name),
      description = state$pipe$description$Title,
      targets = nrow(state$pipe$target_table)
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Get Pipeline Information
#'
#' @description Get detailed information about the currently loaded pipeline
#' including targets, current settings, and available reports.
#' Must call \code{mcp_load_rave_pipeline} first.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether information was retrieved successfully}
#'   \item{name}{Character, pipeline name}
#'   \item{path}{Character, file system path to pipeline directory}
#'   \item{description}{Character, pipeline title/description}
#'   \item{targets}{Data frame of pipeline targets with Names and Description columns}
#'   \item{current_settings}{List, current pipeline settings as key-value pairs}
#'   \item{available_reports}{List, available report templates with metadata}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # MCP example response (truncated for brevity):
#' # {
#' #   "success": true,
#' #   "name": "power_explorer",
#' #   "path": "/.../pipelines/power_explorer",
#' #   "description": "Explore Power or Amplitude of iEEG Spectrogram",
#' #   "targets": {
#' #     "Names": ["settings_path", "settings", "trials_to_export", ...],
#' #     "Description": ["Check settings file", "Load settings", ...]
#' #   },
#' #   "current_settings": {
#' #     "subject_code": "DemoSubject",
#' #     "project_name": "demo",
#' #     "loaded_electrodes": "13-16,24",
#' #     ...
#' #   },
#' #   "available_reports": {
#' #     "univariatePower": {...}
#' #   }
#' # }
#'
#' \dontrun{
#' mcp_load_rave_pipeline("power_explorer")
#' info <- mcp_get_current_rave_pipeline_info()
#' }
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_get_current_rave_pipeline_info <- function() {
  state <- mcp_state()
  pipe <- state$pipe
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use load_rave_pipeline first."))
  }
  list(
    success = TRUE,
    name = pipe$pipeline_name,
    path = pipe$pipeline_path,
    description = pipe$description$Title,
    targets = pipe$target_table,
    current_settings = pipe$get_settings(),
    available_reports = pipe$available_reports
  )
}

#' Set Pipeline Settings
#'
#' @description Configure pipeline settings. This does NOT run the pipeline,
#' only sets parameters. Must call \code{mcp_load_rave_pipeline} first.
#'
#' @param settings_json Character string containing JSON with pipeline settings
#'   as key-value pairs. Use \code{mcp_get_rave_pipeline_info} to see what
#'   settings are available.
#'   Example values: \code{'{"project_name": "demo"}'}, \code{'{"subject_code": "DemoSubject"}'}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether settings were applied successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{current_settings}{List, updated pipeline settings after applying changes}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # MCP example response (truncated for brevity):
#' # {
#' #   "success": true,
#' #   "message": "Settings updated successfully",
#' #   "current_settings": {
#' #     "project_name": "demo",
#' #     "subject_code": "DemoSubject",
#' #     "loaded_electrodes": "13-16,24",
#' #     "epoch_choice": "auditory_onset",
#' #     "condition_variable": "Condition",
#' #     ...
#' #   }
#' # }
#'
#' \dontrun{
#' mcp_load_rave_pipeline("power_explorer")
#' result <- mcp_set_current_rave_pipeline_settings(
#'   '{"project_name": "demo", "subject_code": "DemoSubject"}'
#' )
#' }
#'
#' @keywords mcp-tool mcp-category-configuration
#' @noRd
mcp_set_current_rave_pipeline_settings <- function(settings_json) {
  state <- mcp_state()
  pipe <- state$pipe
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use load_rave_pipeline first."))
  }

  tryCatch({
    settings <- jsonlite::fromJSON(settings_json, simplifyVector = FALSE)
    do.call(pipe$set_settings, settings)
    list(
      success = TRUE,
      message = "Settings updated successfully",
      current_settings = pipe$get_settings()
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Run Pipeline
#'
#' @description Execute the pipeline. This will run computations - request
#' explicit user approval before calling. Can run all targets or specific ones.
#' Execution may take seconds to hours depending on pipeline complexity and data size.
#' Must call \code{mcp_load_rave_pipeline} first.
#'
#' @param target_names \code{Character[]}, specific target names to run.
#'   If empty, runs all targets. Pipeline automatically determines
#'   dependencies, so specifying a target will also run its upstream
#'   dependencies if needed. Default is empty.
#'   Example values: \code{"settings_path"}, \code{"settings"}, \code{"repository"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether pipeline execution completed successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{result_summary}{Data frame summarizing results with columns:
#'     name, type, data, command, depend, seed, path, time, size, bytes,
#'     format, repository, iteration, parent, children, seconds, warnings, error}
#'   \item{error}{Character, error message if success is FALSE}
#'   \item{progress}{List, progress information if execution failed partway through}
#' }
#'
#' @examples
#'
#' # MCP example response:
#' # {
#' #   "success": true,
#' #   "message": "Pipeline execution completed",
#' #   "result_summary": {
#' #     "name": ["settings_path", "settings"],
#' #     "type": ["stem", "stem"],
#' #     "bytes": [1702, 928],
#' #     "seconds": [0.006, 0.000]
#' #   }
#' # }
#'
#' \dontrun{
#' mcp_load_rave_pipeline("power_explorer")
#' mcp_set_current_rave_pipeline_settings('{"project_name": "demo"}')
#' result <- mcp_run_current_rave_pipeline(c("settings_path", "settings"))
#' }
#'
#' @keywords mcp-tool mcp-category-execution mcp-dangerous mcp-requires-approval
#' @noRd
mcp_run_current_rave_pipeline <- function(target_names = NULL) {
  state <- mcp_state()
  pipe <- state$pipe
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use load_rave_pipeline first."))
  }

  tryCatch({
    # Show what will be executed
    targets_to_run <- if (is.null(target_names)) {
      "all targets"
    } else {
      paste(target_names, collapse = ", ")
    }

    message(sprintf("Running pipeline: %s", pipe$pipeline_name))
    message(sprintf("Targets: %s", targets_to_run))

    # Execute
    if (is.null(target_names)) {
      pipe$run()
    } else {
      pipe$run(names = target_names)
    }

    list(
      success = TRUE,
      message = "Pipeline execution completed",
      result_summary = pipe$result_table
    )
  }, error = function(e) {
    list(
      success = FALSE,
      error = conditionMessage(e),
      progress = pipe$progress("summary")
    )
  })
}

#' Get Pipeline Progress
#'
#' @description Check the execution progress of the pipeline. Shows which
#' targets are completed, running, or pending.
#' Must call \code{mcp_load_rave_pipeline} first.
#'
#' @param detail_level Character string, level of detail for progress information.
#'   Either "summary" (overview) or "details" (per-target status).
#'   Default is "summary".
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether progress information was retrieved successfully}
#'   \item{progress}{Object with progress information. Contains a 'format' field
#'     ("structured" for data frame, "text" for string) and progress data.
#'     Summary shows counts (\code{'skipped'}, \code{'dispatched'}, \code{'completed'}, \code{'errored'}, \code{'canceled'}, \code{'since'}),
#'     details shows per-target status}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # MCP example response (structured format):
#' # {
#' #   "success": true,
#' #   "progress": {
#' #     "format": "structured",
#' #     "skipped": 0,
#' #     "dispatched": 0,
#' #     "completed": 2,
#' #     "errored": 0,
#' #     "canceled": 0,
#' #     "since": "moments ago"
#' #   }
#' # }
#'
#' \dontrun{
#' mcp_load_rave_pipeline("power_explorer")
#' mcp_run_current_rave_pipeline(c("settings_path", "settings"))
#' progress <- mcp_get_current_rave_pipeline_progress("summary")
#' }
#'
#' @keywords mcp-tool mcp-category-monitoring
#' @noRd
mcp_get_current_rave_pipeline_progress <- function(detail_level = c("summary", "details")) {
  detail_level <- match.arg(detail_level)
  state <- mcp_state()
  pipe <- state$pipe
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use load_rave_pipeline first."))
  }

  progress <- pipe$progress(method = detail_level)


  # Ensure progress is always an object for consistent JSON output
  # Add format field to indicate structure type
  if (!is.list(progress) && !is.data.frame(progress)) {
    progress <- list(format = "text", value = progress)
  } else {
    # For data.frame, convert to list and add format
    if (is.data.frame(progress)) {
      progress <- as.list(progress)
    }
    progress$format <- "structured"
  }

  list(
    success = TRUE,
    progress = progress
  )
}

#' Read Pipeline Results
#'
#' @description Read and summarize pipeline results. Returns metadata about
#' results rather than full data (which may be large). Use this to understand
#' what outputs are available after pipeline execution.
#' Must call \code{mcp_load_rave_pipeline} first and \code{mcp_run_rave_pipeline}
#' before results are available.
#'
#' @param target_names Character vector of specific target names to read.
#'   If NULL or empty, reads all available results. Default is NULL.
#'   Example values: \code{"settings_path"}, \code{"settings"}, \code{"repository"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether results were read successfully}
#'   \item{results}{List of result summaries, each containing:
#'     name, class, type, size, and summary (structure info for data frames/arrays)}
#'   \item{result_names}{Character vector of result target names}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # MCP example response:
#' # {
#' #   "success": true,
#' #   "results": [
#' #     {
#' #       "name": "settings_path",
#' #       "class": "character",
#' #       "type": "character",
#' #       "size": 120
#' #     },
#' #     {
#' #       "name": "settings",
#' #       "class": "list",
#' #       "type": "list",
#' #       "size": 13400
#' #     }
#' #   ],
#' #   "result_names": ["settings_path", "settings"]
#' # }
#'
#' \dontrun{
#' mcp_load_rave_pipeline("power_explorer")
#' mcp_run_current_rave_pipeline(c("settings_path", "settings"))
#' results <- mcp_read_current_rave_pipeline_results(c("settings_path", "settings"))
#' }
#'
#' @keywords mcp-tool mcp-category-results
#' @noRd
mcp_read_current_rave_pipeline_results <- function(target_names = NULL) {
  state <- mcp_state()
  pipe <- state$pipe
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use load_rave_pipeline first."))
  }

  tryCatch({
    if (is.null(target_names)) {
      results <- pipe$read()
    } else {
      results <- pipe$read(target_names)
    }

    # Convert results to JSON-serializable format
    results_summary <- lapply(names(results), function(name) {
      obj <- results[[name]]
      list(
        name = name,
        class = class(obj),
        type = typeof(obj),
        size = utils::object.size(obj),
        summary = if (is.data.frame(obj)) {
          list(rows = nrow(obj), cols = ncol(obj), names = names(obj))
        } else if (is.array(obj)) {
          list(dim = dim(obj), dimnames = dimnames(obj))
        } else {
          utils::str(obj, max.level = 1, give.attr = FALSE)
        }
      )
    })

    list(
      success = TRUE,
      results = results_summary,
      result_names = names(results)
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

