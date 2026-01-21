
# MCP Tool Implementation Functions
# These functions are called by MCP tools to interact with RAVE pipelines
# Each function is documented with \pkg{roxygen2} tags that are used to generate
# the tool specifications in `inst/mcp`

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

#' # Direct implementation:
#'
#' # The tool invokes an internal call to `ravepipeline:::mcp_list_rave_pipelines`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#'
#' pipelines <- ravepipeline::pipeline_list()
#' list(pipelines = pipelines, count = length(pipelines))
#'
#' # MCP example response:
#' # {
#' #   "pipelines": [
#' #     "power_explorer", "notch_filter", "wavelet_module", "..."
#' #   ],
#' #   "count": 18
#' # }
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
#' for configuration and execution. Stores the pipeline instance in internal
#' state for use by subsequent MCP tool calls.
#'
#' @param pipeline_name Character string, name of the pipeline to load.
#'   Use \code{ravepipeline-mcp_list_rave_pipelines} to see available options.
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
#' # Direct implementation:
#' # The tool invokes an internal call to `ravepipeline:::mcp_load_rave_pipeline`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Use 'pipe' variable to avoid confusion with the pipeline() function
#' pipe <- ravepipeline::pipeline("power_explorer")
#' list(
#'   success = TRUE,
#'   message = sprintf("Loaded pipeline: %s", "power_explorer"),
#'   description = pipe$description$Title,
#'   targets = nrow(pipe$target_table)
#' )
#'
#' @keywords mcp-tool mcp-category-setup
#' @noRd
mcp_load_rave_pipeline <- function(pipeline_name, .state_env = fastmap::fastmap()) {
  tryCatch({
    pipe <- pipeline(pipeline_name)
    .state_env$set("pipeline", pipe)

    list(
      success = TRUE,
      message = sprintf("Loaded pipeline: %s", pipeline_name),
      description = pipe$description$Title,
      targets = nrow(pipe$target_table)
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Get Pipeline Information
#'
#' @description Get detailed information about the currently loaded pipeline
#' including targets, current settings, and available reports.
#' Must call \code{ravepipeline-mcp_load_rave_pipeline} first.
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
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_get_current_rave_pipeline_info`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Prepare: load the pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#'
#' list(
#'   success = TRUE,
#'   name = pipe$pipeline_name,
#'   path = pipe$pipeline_path,
#'   description = pipe$description$Title,
#'   targets = head(pipe$target_table, 3),    # Truncated for display
#'   current_settings = head(pipe$get_settings(), 3), # Truncated
#'   available_reports = head(pipe$available_reports, 2) # Truncated
#' )
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_get_current_rave_pipeline_info <- function(.state_env = fastmap::fastmap()) {
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_load_rave_pipeline first."))
  }
  res <- list(
    success = TRUE,
    name = pipe$pipeline_name,
    path = pipe$pipeline_path,
    description = pipe$description$Title,
    targets = pipe$target_table,
    current_settings = pipe$get_settings(),
    available_reports = pipe$available_reports
  )

  res

}

#' Set Pipeline Settings
#'
#' @description Configure pipeline settings. This does NOT run the pipeline,
#' only sets parameters. Must call `ravepipeline-mcp_load_rave_pipeline` first.
#'
#' @param settings_json Character string containing JSON with pipeline settings
#'   as key-value pairs. Use `ravepipeline-mcp_get_current_rave_pipeline_info` to see
#'   what settings are available.
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
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_set_current_rave_pipeline_settings`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Load pipeline with pipeline name
#' pipe <- ravepipeline::pipeline("power_explorer")
#'
#' # Then set input settings
#' pipe$set_settings(project_name = "demo", subject_code = "DemoSubject")
#'
#' # Construct response (optional for users)
#' list(
#'   success = TRUE,
#'   message = "Settings updated successfully",
#'   current_settings = pipe$get_settings()
#' )
#'
#' @keywords mcp-tool mcp-category-configuration
#' @noRd
mcp_set_current_rave_pipeline_settings <- function(settings_json, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_load_rave_pipeline first."))
  }

  tryCatch({
    settings <- jsonlite::fromJSON(settings_json, simplifyVector = FALSE)
    pipe$set_settings(.list = settings)
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
#' Use `ravepipeline-mcp_get_current_rave_pipeline_info` to discover valid target names.
#' Execution may take seconds to hours depending on pipeline complexity and data size.
#' Must call \code{ravepipeline-mcp_load_rave_pipeline} first.
#'
#' @param target_names \code{Character[]}, specific target names to run.
#'   If empty, runs all targets. Pipeline automatically determines
#'   dependencies, so specifying a target will also run its upstream
#'   dependencies if needed. Default is empty.
#'   Do not guess names; check valid targets first.
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
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_run_current_rave_pipeline`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer" on "demo/DemoSubject"
#' # subject ID (A RAVE subject ID has <project/subject> format)
#'
#' # Prepare: load & set pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#' pipe$set_settings(project_name = "demo", subject_code = "DemoSubject")
#'
#' # Run specific target(s)
#' pipe$run(names = c("settings"), return_values = FALSE)
#'
#' list(
#'   success = TRUE,
#'   message = "Pipeline execution completed",
#'   result_summary = head(pipe$result_table, 3) # Truncated
#' )
#'
#' @keywords mcp-tool mcp-category-execution mcp-dangerous mcp-requires-approval
#' @noRd
mcp_run_current_rave_pipeline <- function(target_names = NULL, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_load_rave_pipeline first."))
  }

  tryCatch({
    # Show what will be executed
    targets_to_run <- if (is.null(target_names)) {
      "all targets"
    } else {
      paste(target_names, collapse = ", ")
    }

    logger(sprintf("Running pipeline: %s", pipe$pipeline_name), level = "info")
    logger(sprintf("Targets: %s", targets_to_run), level = "info")

    # Execute
    if (is.null(target_names)) {
      pipe$run(return_values = FALSE)
    } else {
      pipe$run(names = target_names, return_values = FALSE)
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
#' Must call \code{ravepipeline-mcp_load_rave_pipeline} first.
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
#'
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_get_current_rave_pipeline_progress`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Prepare: load, set & run pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#' pipe$set_settings(project_name = "demo", subject_code = "DemoSubject")
#' pipe$run(names = c("settings"), return_values = FALSE)
#'
#' # Check progress
#' pipe$progress(method = "details")
#'
#'
#'
#' @keywords mcp-tool mcp-category-monitoring
#' @noRd
mcp_get_current_rave_pipeline_progress <- function(detail_level = c("summary", "details"),
                                                   .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))

  detail_level <- match.arg(detail_level)
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_load_rave_pipeline first."))
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
#' @description This MCP tool returns summary metadata about pipeline results,
#' not the actual data (which may be large). For direct R code generation, use
#' \code{pipe$read('target_name', simplify = FALSE)} to get the full data object.
#' If unsure which result names are available, first call
#' \code{ravepipeline-mcp_get_current_rave_pipeline_info} and check the 'targets'
#' field (Names column). Do not guess target names. Must call
#' \code{ravepipeline-mcp_load_rave_pipeline} first and
#' \code{ravepipeline-mcp_run_current_rave_pipeline} before results are available.
#'
#' @param target_names Character vector of specific target names to read.
#'   - Single target: \code{"settings"} - returns summary for one result
#'   - Multiple targets: \code{c("settings", "repository")} - returns summary for specific results
#'   - NULL or empty (default): reads ALL available results (WARNING: may be slow and memory-intensive)
#'   To discover available target names, use \code{ravepipeline-mcp_get_current_rave_pipeline_info}
#'   and check the 'targets' field for Names. Do not guess or read all results for discovery.
#'   Example values: \code{"settings_path"}, \code{"settings"}, \code{"repository"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether results were read successfully}
#'   \item{result_summary}{List of result summaries, each containing:
#'     name, class, type, size, and summary (structure info for data frames/arrays)}
#'   \item{result_names}{Character vector of result target names}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_read_current_rave_pipeline_results`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer" and one of its building target "settings"
#'
#' # Prepare: load, set & run pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#' pipe$run(names = "settings")
#'
#' # RECOMMENDED: Use simplify = FALSE for consistent output structure
#' # This is especially important for FileArray objects, which should NOT be simplified
#' results <- pipe$read("settings", simplify = FALSE)
#'
#' # Note: simplify = TRUE (default) may return different types depending on result count
#' # Always use simplify = FALSE when generating code for reproducibility
#'
#' results
#'
#'
#'
#' @keywords mcp-tool mcp-category-results
#' @noRd
mcp_read_current_rave_pipeline_results <- function(target_names = NULL, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))

  pipe <- .state_env$get("pipeline")

  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_load_rave_pipeline first."))
  }

  tryCatch({
    if (!length(target_names)) {
      results <- pipe$read()
    } else {
      results <- pipe$read(target_names, simplify = FALSE)
    }

    # Convert results to JSON-serializable format
    result_summary <- mcp_describe(results)

    # results_summary <- lapply(names(results), function(name) {
    #   obj <- results[[name]]
    #   list(
    #     name = name,
    #     class = class(obj),
    #     type = typeof(obj),
    #     size = utils::object.size(obj),
    #     summary = if (is.data.frame(obj)) {
    #       list(rows = nrow(obj), cols = ncol(obj), names = names(obj))
    #     } else if (is.array(obj)) {
    #       list(dim = dim(obj), dimnames = dimnames(obj))
    #     } else {
    #       utils::str(obj, max.level = 1, give.attr = FALSE)
    #     }
    #   )
    # })

    list(
      success = TRUE,
      result_summary = result_summary,
      result_names = names(results)
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Configure MCP Tool Output Settings
#'
#' @description
#' (This function is intended for MCP calls directly, not for end-users).
#' Adjusts runtime settings for MCP tool output formatting. Controls when
#' results are returned as JSON versus formatted text. When JSON serialization
#' fails (due to size or complexity), tools automatically fall back to
#' human-readable text output via \code{\link{mcp_describe}}.
#'
#' @param max_size_for_json_output Integer. Maximum object size (in bytes)
#'   for JSON serialization attempt. Default is \code{NULL} (no change).
#'   When set, objects larger than this threshold will use text formatting.
#'   Recommended range: 5120-102400 bytes. Set to \code{Inf} to always
#'   attempt JSON serialization (not recommended).
#' @param max_print_lines Integer. Maximum number of lines for text output
#'   when using \code{\link{mcp_describe}}. Default is \code{NULL} (no change).
#'   Controls verbosity of fallback text formatting. Typical range: 50-200.
#' @param .state_env Environment. Internal state container (automatically
#'   provided by MCP framework).
#'
#' @return List containing:
#' \describe{
#'   \item{success}{Logical, always \code{TRUE}}
#'   \item{message}{Character, confirmation message}
#'   \item{settings}{List, all current output settings with their values}
#' }
#'
#' @examples
#'
#' # This function is intended for MCP calls directly, not for end-users
#' ravepipeline:::mcp_configure_output_settings()
#'
#' @keywords mcp-tool mcp-category-configuration
#' @noRd
mcp_configure_output_settings <- function(
    max_size_for_json_output = NULL,
    max_print_lines = NULL,
    .state_env = fastmap::fastmap()
) {
  updated <- character(0)

  # Update max_size_for_json_output if provided
  if (!is.null(max_size_for_json_output)) {
    if (!is.numeric(max_size_for_json_output) || length(max_size_for_json_output) != 1) {
      return(list(
        success = FALSE,
        error = "max_size_for_json_output must be a single numeric value"
      ))
    }
    if (!is.infinite(max_size_for_json_output) && max_size_for_json_output < 1024) {
      return(list(
        success = FALSE,
        error = "max_size_for_json_output must be >= 1024 bytes or Inf"
      ))
    }
    .state_env$.max_size_for_json_output <- as.numeric(max_size_for_json_output)
    updated <- c(updated, "max_size_for_json_output")
  }

  # Update max_print_lines if provided
  if (!is.null(max_print_lines)) {
    if (!is.numeric(max_print_lines) || length(max_print_lines) != 1) {
      return(list(
        success = FALSE,
        error = "max_print_lines must be a single numeric value"
      ))
    }
    if (max_print_lines < 10) {
      return(list(
        success = FALSE,
        error = "max_print_lines must be >= 10"
      ))
    }
    .state_env$.max_print_lines <- as.integer(max_print_lines)
    updated <- c(updated, "max_print_lines")
  }

  # Get current settings (with defaults)
  current_max_size <- .state_env$.max_size_for_json_output
  if (is.null(current_max_size) || length(current_max_size) != 1 || is.na(current_max_size)) {
    current_max_size <- getOption("ravepipeline.mcp.max_size_for_json_output", 20480)
  }

  current_max_lines <- .state_env$.max_print_lines
  if (is.null(current_max_lines) || length(current_max_lines) != 1 || is.na(current_max_lines)) {
    current_max_lines <- 100
  }

  # Build response
  message <- if (length(updated) > 0) {
    sprintf("Output settings updated: %s", paste(updated, collapse = ", "))
  } else {
    "No settings changed (returning current configuration)"
  }

  list(
    success = TRUE,
    message = message,
    settings = list(
      max_size_for_json_output = as.numeric(current_max_size),
      max_print_lines = as.integer(current_max_lines)
    )
  )
}
