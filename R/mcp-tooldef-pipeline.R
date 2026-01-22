
# MCP Tool Implementation Functions
# These functions are called by MCP tools to interact with RAVE pipelines
# Each function is documented with \pkg{roxygen2} tags that are used to generate
# the tool specifications in `inst/mcp`

#' List Available RAVE Pipelines
#'
#' @description List all installed RAVE pipelines available on the system.
#'
#' ## When to Use
#' Always start here - never assume pipeline names. This is the first step
#' in any pipeline workflow.
#'
#' ## Best Practices
#' - DO: Call this before attempting to load any pipeline
#' - DO: Use exact names from the returned list when loading pipelines
#' - DON'T: Guess or assume pipeline names exist
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_list`
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
mcp_tool_pipeline_list <- function() {
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
#' ## When to Use
#' After identifying a pipeline from \code{ravepipeline-mcp_tool_pipeline_list},
#' before any other operations.
#'
#' ## Preconditions
#' - Must know the exact pipeline name from \code{mcp_tool_pipeline_list}
#'
#' ## Best Practices
#' - DO: Always call \code{mcp_tool_pipeline_list} first to get exact names
#' - DO: Required before get_info, set_settings, or run
#' - DON'T: Load pipelines with guessed names
#'
#' @param pipeline_name Character string, name of the pipeline to load.
#'   Use \code{ravepipeline-mcp_tool_pipeline_list} to see available options.
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_load`
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
mcp_tool_pipeline_load <- function(pipeline_name, .state_env = fastmap::fastmap()) {
  tryCatch({
    pipe <- pipeline(pipeline_name)
    env <- pipe$shared_env()
    .state_env$set("pipeline", pipe)
    .state_env$set("shared_env", env)

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
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' Need parameter schema, validation rules, pipeline details, or to discover
#' available result target names.
#'
#' ## Preconditions
#' - Pipeline must be loaded first
#'
#' ## Best Practices
#' - DO: Check targets$Names to discover available result names (don't call read without arguments)
#' - DO: Use current_settings as reference for parameter names and structure
#' - DO: Validate parameter names and types against schema before setting
#' - DO: Use this to discover required parameters before configuration
#' - DON'T: Guess target names or parameter names
#' - DON'T: Read all results to discover names (expensive and slow)
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_get_info`
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
mcp_tool_pipeline_get_info <- function(.state_env = fastmap::fastmap()) {
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
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

#' Get RAVE Pipeline Target Dependencies
#'
#' @description Get the dependency structure of pipeline targets. Returns a
#' list mapping each target name to its dependencies. This is useful for
#' understanding the computational graph and execution order.
#' RAVE's \code{pipeline$run} method automatically figures out which dependent
#' targets need to be executed when you run any specific target - you don't
#' need to manually specify dependencies to run.
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' Need to understand target relationships, execution order, or which targets
#' will run together. When planning partial pipeline execution, obtaining
#' target by-products (since the dependencies are automatically evaluated,
#' hence the results will be available) or debugging execution issues.
#'
#' ## Preconditions
#' - Pipeline must be loaded: use `ravepipeline-mcp_tool_pipeline_load` first
#'
#' ## Best Practices
#' - DO: Use this to understand what will execute when running specific targets
#' - DO: Check dependencies before running expensive targets
#' - DO: Use to explain to users which targets will be executed together, so
#'      you can avoid re-running the targets that have been already evaluated
#' - DON'T: Manually manage dependencies (`ravepipeline-mcp_tool_pipeline_run`
#'      handles this automatically)
#' - DON'T: Assume you know execution order without checking dependencies
#'
#' @param target_names Character vector, optional specific target names to get
#'   dependencies for. If \code{NULL} or empty (default), returns dependencies
#'   for all targets. However, it is strongly recommended that you specify the target
#'   name(s)
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether dependency information was retrieved successfully}
#'   \item{dependencies}{Named list where each name is a target and its value is a
#'     character vector of target names it depends on. Empty vectors indicate
#'     targets with no dependencies (leaf nodes in the computation graph)}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Complete example:
#' # The tool invokes an internal call to
#' # `ravepipeline:::mcp_tool_pipeline_get_target_dependencies`
#' # From within R. If user asks for the implementations, here is
#' # how `dependencies` from the output gets generated:
#'
#' # Prepare: load the pipeline: using one builtin module as an example
#' pipe <- ravepipeline::pipeline("power_explorer")
#'
#' # Get `dependencies`: names are targets and values are the dependence -
#' #   other targets that will run when evaluating this target
#' pipe$visualize(glimpse = TRUE)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_pipeline_get_target_dependencies <- function(target_names = NULL, .state_env = fastmap::fastmap()) {
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
  }
  deps <- pipe$visualize(glimpse = TRUE)

  if(length(target_names)) {
    deps <- deps[target_names]
  }

  list(
    success = TRUE,
    dependencies = deps
  )
}

#' Set Pipeline Settings
#'
#' @description Configure pipeline settings. This does NOT run the pipeline,
#' only sets parameters. Must call `ravepipeline-mcp_tool_pipeline_load` first.
#'
#' ## When to Use
#' User provides parameter values or you need to configure analysis settings.
#'
#' ## Preconditions
#' - Pipeline must be loaded
#' - Parameters must be validated against schema from \code{mcp_tool_pipeline_get_info}
#'
#' ## Best Practices
#' - DO: Always call \code{mcp_tool_pipeline_get_info} first to get parameter schema
#' - DO: Validate all parameters against schema (types, ranges, enums)
#' - DO: Use current_settings from get_info as reference
#' - DO: Confirm settings with user before execution
#' - DON'T: Set parameters without checking schema first
#' - DON'T: Pass parameters without validation
#' - DON'T: Guess parameter names (e.g., 'baseline = c(-1, 0)' without verification)
#'
#' @param settings_json Character string containing JSON with pipeline settings
#'   as key-value pairs. Use `ravepipeline-mcp_tool_pipeline_get_info` to see
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_set_settings`
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
mcp_tool_pipeline_set_settings <- function(settings_json, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
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
#' Use `ravepipeline-mcp_tool_pipeline_get_info` to discover valid target names.
#' Execution may take seconds to hours depending on pipeline complexity and data size.
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' Only when user confirms they want to execute the analysis.
#'
#' ## Preconditions
#' - Pipeline must be loaded
#' - Settings must be configured via \code{mcp_tool_pipeline_set_settings}
#' - User must explicitly approve execution
#'
#' ## Best Practices
#' - DO: Present action details to user (tool, purpose, parameters, expected outcomes)
#' - DO: Wait for explicit user confirmation - NEVER auto-approve
#' - DO: Confirm execution started after approval
#' - DON'T: Execute without explicit user consent
#' - DON'T: Auto-approve dangerous operations
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_run`
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
mcp_tool_pipeline_run <- function(target_names = NULL, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
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
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' After starting execution with \code{mcp_tool_pipeline_run}, to track progress.
#'
#' ## Preconditions
#' - Pipeline execution has been started
#'
#' ## Best Practices
#' - DO: Poll every 5 seconds until status is 'completed'
#' - DO: Report progress to user (e.g., "Processing: 45% complete...")
#' - DO: Confirm completion (e.g., "Analysis finished successfully!")
#' - DON'T: Start pipeline and assume it completed without checking
#' - DON'T: Ignore long-running operations
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_get_progress`
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
mcp_tool_pipeline_get_progress <- function(detail_level = c("summary", "details"),
                                                   .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))

  detail_level <- match.arg(detail_level)
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
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
#' \code{ravepipeline-mcp_tool_pipeline_get_info} and check the 'targets'
#' field (Names column). Do not guess target names. Must call
#' \code{ravepipeline-mcp_tool_pipeline_load} first and
#' \code{ravepipeline-mcp_tool_pipeline_run} before results are available.
#'
#' ## When to Use
#' Pipeline execution completed successfully and you need to retrieve results.
#'
#' ## Preconditions
#' - Pipeline execution must be completed
#' - Must know target names from \code{mcp_tool_pipeline_get_info}
#'
#' ## Best Practices
#' - DO: First call \code{mcp_tool_pipeline_get_info} to see 'targets' field
#' - DO: Always specify target_names when possible (not NULL)
#' - DO: Provide context and interpretation with results, not just raw data
#' - DON'T: Guess target names
#' - DON'T: Call without target_names (extracts ALL results - memory-intensive and slow)
#' - DON'T: Read all results for discovery (use get_info instead)
#' - DON'T: Return raw data dumps without interpretation
#'
#' @param target_names Character vector of specific target names to read.
#'   - Single target: \code{"settings"} - returns summary for one result
#'   - Multiple targets: \code{c("settings", "repository")} - returns summary for specific results
#'   - NULL or empty (default): reads ALL available results (WARNING: may be slow and memory-intensive)
#'   To discover available target names, use \code{ravepipeline-mcp_tool_pipeline_get_info}
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
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_read_results`
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
mcp_tool_pipeline_read_results <- function(target_names = NULL, .state_env = fastmap::fastmap()) {
  # DEBUG
  # .state_env = fastmap::fastmap()
  # .state_env$set("pipeline", ravepipeline::pipeline("power_explorer"))

  pipe <- .state_env$get("pipeline")

  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
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

#' Get Current Pipeline Main Script
#'
#' @description Read the main workflow document (main.Rmd) of the currently
#' loaded pipeline. This document describes the pipeline workflow steps and
#' computational targets. Only supports R Markdown workflows.
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' Need to understand pipeline workflow, computational steps, or documentation.
#' When user asks "how does this pipeline work?" or needs implementation details.
#'
#' ## Preconditions
#' - Pipeline must be loaded
#' - Pipeline must have an R Markdown workflow (main.Rmd)
#'
#' ## Best Practices
#' - DO: Use pattern parameter to extract specific sections (e.g., "^# " for headers)
#' - DO: Use pattern to reduce output size and save tokens
#' - DO: Search for specific keywords (e.g., "wavelet|frequency|baseline")
#' - DON'T: Read entire main.Rmd without pattern (will be large and waste tokens)
#' - DON'T: Assume you know implementation details without checking documentation
#'
#' @param pattern Character string, optional regular expression pattern (R format)
#'   to search for specific lines in the document. If provided, only matching
#'   lines and surrounding context will be returned (will be using `grepl` in R).
#'   If \code{NULL} or empty (default), the entire document is returned.
#'   WARNING: Reading the entire document may result in large output.
#'   Example values: `"^# "` (section headers), `"\\{rave.*\\}"` (target definitions).
#' @param context_size Integer, number of lines to include before and after
#'   each matching line when using \code{pattern}. Default is 5.
#'   Only used when \code{pattern} is specified.
#'   Example values: \code{3}, \code{10}, \code{20}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the script was read successfully}
#'   \item{path}{Character, absolute path to main.Rmd file}
#'   \item{content}{Character vector, lines of the R Markdown document (full or filtered)}
#'   \item{line_count}{Integer, total number of lines in the document}
#'   \item{returned_lines}{Integer, number of lines returned (may differ from line_count when using pattern)}
#'   \item{matches}{Integer, number of lines matching the pattern (only when pattern is used)}
#'   \item{match_info}{Data frame with columns \code{line_number} and \code{matched_text} (only when pattern is used)}
#'   \item{warning}{Character, warning message if returning full document (which may be large)}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_get_script`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Prepare: load the pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#'
#' # Example: Search for RAVE target code blocks with context
#' main_path <- file.path(pipe$pipeline_path, "main.Rmd")
#' all_content <- readLines(main_path, warn = FALSE)
#' pattern <- "^```\\{rave"
#' context_size <- 3
#'
#' # Find matching line numbers
#' match_lines <- grep(pattern, all_content)
#'
#' # Expand to include context
#' line_ranges <- lapply(match_lines, function(i) {
#'   start <- max(1, i - context_size)
#'   end <- min(length(all_content), i + context_size)
#'   start:end
#' })
#' unique_lines <- sort(unique(unlist(line_ranges)))
#'
#' list(
#'   success = TRUE,
#'   path = main_path,
#'   content = all_content[unique_lines],
#'   line_count = length(all_content),
#'   returned_lines = length(unique_lines),
#'   matches = length(match_lines),
#'   match_info = data.frame(
#'     line_number = match_lines,
#'     matched_text = all_content[match_lines]
#'   )
#' )
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_pipeline_get_script <- function(
    pattern = NULL,
    context_size = 5,
    .state_env = fastmap::fastmap()
) {
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
  }

  tryCatch({
    # Get main.Rmd path
    main_path <- file.path(pipe$pipeline_path, "main.Rmd")

    if(!file.exists(main_path)) {
      return(list(
        success = FALSE,
        error = sprintf("main.Rmd not found at %s. This pipeline does not have an R Markdown workflow.", main_path)
      ))
    }

    # Read full content
    all_content <- readLines(main_path, warn = FALSE)
    total_lines <- length(all_content)

    # If no pattern, return full document with warning
    if(is.null(pattern) || length(pattern) == 0 || pattern == "") {
      return(list(
        success = TRUE,
        path = main_path,
        content = all_content,
        line_count = total_lines,
        returned_lines = total_lines,
        warning = "Returning full document which may be large. Consider using pattern parameter to filter specific sections."
      ))
    }

    # Search for pattern
    match_lines <- grep(pattern, all_content, perl = TRUE)

    if(length(match_lines) == 0) {
      return(list(
        success = TRUE,
        path = main_path,
        content = character(0),
        line_count = total_lines,
        returned_lines = 0,
        matches = 0,
        match_info = data.frame(line_number = integer(0), matched_text = character(0)),
        warning = sprintf("No lines matched pattern: %s", pattern)
      ))
    }

    # Expand each match to include context
    line_ranges <- lapply(match_lines, function(i) {
      start <- max(1, i - context_size)
      end <- min(total_lines, i + context_size)
      start:end
    })

    # Get unique line numbers and sort
    unique_lines <- sort(unique(unlist(line_ranges)))

    # Extract content for these lines
    filtered_content <- all_content[unique_lines]

    # Build match info
    match_info <- data.frame(
      line_number = match_lines,
      matched_text = all_content[match_lines],
      stringsAsFactors = FALSE
    )

    list(
      success = TRUE,
      path = main_path,
      content = filtered_content,
      line_count = total_lines,
      returned_lines = length(unique_lines),
      matches = length(match_lines),
      match_info = match_info
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Get Current Pipeline Helper Functions
#'
#' @description Load and discover helper functions from the currently loaded
#' pipeline's shared environment using pipeline method `pipe$shared_env()`
#' method. Returns function signatures (names
#' and arguments) to help AI understand what functions are available without
#' loading full implementations. The shared environment is stored in state
#' for potential reuse by other tools.
#' Must call \code{ravepipeline-mcp_tool_pipeline_load} first.
#'
#' ## When to Use
#' **MANDATORY**: When user requests custom code generation, pipeline result
#' reproduction, or data manipulation/customization. MUST call this FIRST to
#' discover available utilities before writing any custom code.
#'
#' **OPTIONAL**: When only running pipelines, getting info, or reading results
#' without customization. Not needed for standard pipeline operations.
#'
#' ## Preconditions
#' - Pipeline must be loaded
#'
#' ## Best Practices
#' - DO: Always discover helpers FIRST when generating custom code
#' - DO: Use \code{pattern} parameter to filter by keywords (e.g., \code{"plot"}, \code{"condition"})
#' - DO: Reuse helpers if their names/signatures suggest relevance
#' - NOTE: Using discovered helpers is optional if you're unsure about their use-cases,
#'   but the discovery step itself is required to avoid reinventing existing functionality
#' - DON'T: Write custom code without discovering helpers first
#' - DON'T: Skip this tool when user requests code generation or customization
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether helper functions were loaded successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{functions}{List of function signatures, each containing:
#'     \code{name} (function name), \code{signature} (formatted arguments),
#'     and \code{formals} (list of argument names and defaults)}
#'   \item{count}{Integer, total number of helper functions found}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Complete example:
#' # The tool invokes an internal call to `ravepipeline:::mcp_tool_pipeline_get_helpers`
#' # From within R. While users should not execute the function directly,
#' # this function has the following equivalent implementation:
#' # Example using pipeline_name called "power_explorer"
#'
#' # Prepare: load the pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#'
#' # Load shared environment
#' env <- pipe$shared_env()
#'
#' # Get all ordinary object names from environment
#' all_names <- ls(env, all.names = FALSE)
#' all_names <- all_names[grepl("^[a-zA-Z]", all_names)]
#'
#' # Filter for functions only
#' func_names <- all_names[vapply(all_names, function(nm) { is.function(env[[nm]]) }, FALSE)]
#'
#' # Apply pattern filter if provided (example with pattern = "plot")
#' # func_names <- func_names[grepl("plot", func_names, ignore.case = TRUE)]
#'
#' # Extract function signatures
#' function_signatures <- lapply(func_names, function(name, ...) {
#'   fn <- env[[name]]
#'   body(fn) <- quote({})
#'   signature <- sprintf(
#'     "helpers$%s <- %s",
#'     name,
#'     paste(trimws(deparse(fn)), collapse = " ")
#'   )
#'
#'   signature
#' })
#' function_signatures <- unlist(function_signatures)
#'
#' list(
#'   success = TRUE,
#'   message = "Loaded helper function environment with R code `helpers <- pipe$shared_env()`",
#'   # The actual results will show all signatures. This example only shows three to save space.
#'   functions = function_signatures[seq_len(3)],
#'   count = length(function_signatures)
#' )
#'
#' @param pattern Character string, optional regex pattern to filter function
#'   names. Use this to find specific categories of helpers (e.g., \code{"plot|draw"}
#'   for plotting functions, \code{"condition"} for condition-related functions).
#'   Pattern matching is case-insensitive. If \code{NULL} (default), returns all
#'   helper functions.
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_pipeline_get_helpers <- function(pattern = NULL, .state_env = fastmap::fastmap()) {
  pipe <- .state_env$get("pipeline")
  if(is.null(pipe)) {
    return(list(success = FALSE, error = "No pipeline loaded. Use ravepipeline-mcp_tool_pipeline_load first."))
  }

  env <- .state_env$get("shared_env", {
    env <- pipe$shared_env()
    .state_env$set("shared_env", env)
    env
  })

  tryCatch({
    # Load shared environment
    logger("Loading shared environment for pipeline helper functions...", level = "info")

    # Get all object names from environment
    all_names <- ls(env, all.names = FALSE)
    all_names <- all_names[grepl("^[a-zA-Z]", all_names)]

    # Filter for functions only
    func_names <- all_names[vapply(all_names, function(nm) { is.function(env[[nm]]) }, FALSE)]
    
    # Apply pattern filter if provided
    if (!is.null(pattern) && nzchar(pattern)) {
      func_names <- func_names[grepl(pattern, func_names, ignore.case = TRUE)]
    }

    # Extract function signatures
    function_signatures <- lapply(func_names, function(name, ...) {
      fn <- env[[name]]
      body(fn) <- quote({})
      signature <- sprintf(
        "helpers$%s <- %s",
        name,
        paste(trimws(deparse(fn)), collapse = " ")
      )

      signature
    })
    function_signatures <- unlist(function_signatures)

    list(
      success = TRUE,
      message = "Loaded helper function environment with R code `helpers <- pipe$shared_env()`",
      functions = function_signatures,
      count = length(function_signatures)
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}
