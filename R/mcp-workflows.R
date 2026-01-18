# RAVE MCP Workflows System
# Workflow loading, saving, validation, and S3 class definitions

#' List RAVE MCP Workflow Names
#'
#' @description Lists all available MCP workflow names from a package or
#' directory. Returns workflow names with source information (path and
#' package) as attributes.
#'
#' @param pkg Character. Package name. If missing, must specify path directly
#' @param path Character. Path to workflows directory.
#'   Default is system.file("mcp", "workflows", package = pkg)
#'
#' @return Character vector of workflow names with attribute "source_from"
#'   containing a list with "path" and "pkg" (if applicable)
#'
#' @examples
#' # List workflows from package
#' workflows <- mcpflow_list("ravepipeline")
#'
#' # Get source information
#' attr(workflows, "source_from")
#'
#'
#' # List from custom path
#' path <- system.file("mcp", "workflows",
#'                      package = "ravepipeline")
#'
#' mcpflow_list(path = path)
#'
#' @export
mcpflow_list <- function(pkg, path = system.file("mcp", "workflows", package = pkg)) {
  # Track if pkg was provided
  pkg_provided <- !missing(pkg)

  # If pkg is missing, user must specify path
  if (!pkg_provided && missing(path)) {
    stop("Either 'pkg' or 'path' must be provided")
  }

  # Validate path
  if (path == "") {
    if (pkg_provided) {
      stop("Package '", pkg, "' does not have any RAVE MCP workflow files")
    } else {
      stop("Invalid path: empty string")
    }
  }

  if (!dir.exists(path)) {
    stop("No MCP workflows found in ", path)
  }

  # Find all YAML files
  yaml_files <- list.files(path, pattern = "\\.yaml$", full.names = TRUE)

  if (length(yaml_files) == 0) {
    result <- character(0)
    attr(result, "source_from") <- list(
      path = path,
      pkg = if (pkg_provided) pkg else NULL
    )
    return(result)
  }

  # Extract workflow names from files
  workflow_names <- vapply(yaml_files, function(f) {
    tryCatch(
      {
        wf <- yaml::read_yaml(f)
        wf$name %||% tools::file_path_sans_ext(basename(f))
      },
      error = function(e) {
        tools::file_path_sans_ext(basename(f))
      }
    )
  }, character(1))

  workflow_names <- unname(workflow_names)

  # Attach source information
  attr(workflow_names, "source_from") <- list(
    path = path,
    pkg = if (pkg_provided) pkg else NULL
  )

  workflow_names
}

#' Read RAVE MCP Workflow
#'
#' @description Reads and parses a single MCP workflow from various sources.
#'
#' @param x Source specification: character path, "package::name",
#'   package namespace (environment), or workflow object
#' @param tools Logical. Whether to load referenced MCP tools. Default is TRUE
#' @param work_dir Character. Base directory containing tools/ subdirectory for
#'   loading local tools. Default is "." (current directory). For file paths,
#'   if NULL, infers from the workflow file location.
#' @param ... Additional arguments passed to methods
#'
#' @return A workflow object with class "ravepipeline_mcp_workflow"
#'
#' @examples
#' # Read from package
#' wf <- mcpflow_read("ravepipeline:::rave_pipeline_class_guide")
#'
#' # Read from file path
#' path <- system.file(
#'   "mcp", "workflows", "rave_pipeline_class_guide.yaml",
#'   package = "ravepipeline"
#' )
#' wf <- mcpflow_read(path)
#'
#'
#' @export
mcpflow_read <- function(x, tools = TRUE, tools_dir = NULL, ...) {
  UseMethod("mcpflow_read")
}

#' @export
mcpflow_read.environment <- function(x, tools = TRUE, tools_dir = NULL, name = NA, ...) {
  # x is a namespace (package)
  pkg_name <- environmentName(x)

  if (length(name) != 1 || is.na(name)) {
    # List all workflows to show what's available
    workflows <- mcpflow_list(pkg_name)
    if (length(workflows) == 0) {
      stop("No workflows found in package '", pkg_name, "'")
    }
    # Fail loudly - require explicit workflow name
    stop(
      "Workflow 'name' must be specified. Available workflows in package '",
      pkg_name, "': ", paste("\n  -", workflows, collapse = "")
    )
  }

  # Load workflow from package
  workflow_path <- system.file("mcp", "workflows", paste0(name, ".yaml"),
    package = pkg_name
  )
  if (workflow_path == "") {
    stop("Workflow '", name, "' not found in package '", pkg_name, "'")
  }

  # For package workflows, use package's mcp directory as work_dir
  tools_dir <- system.file("mcp", "tools", package = pkg_name)
  mcpflow_read.character(workflow_path, tools = tools, tools_dir = tools_dir, ...)
}

#' @export
mcpflow_read.character <- function(x, tools = TRUE, tools_dir = NULL, ...) {
  # Check if x is "package::name" or "package:::name" format
  if (grepl("[:-]{2,3}", x)) {
    parts <- strsplit(x, "[:-]{2,3}")[[1]]
    if (length(parts) == 2) {
      pkg_name <- trimws(parts[1])
      workflow_name <- trimws(parts[2])

      # Load from package
      workflow_path <- system.file("mcp", "workflows",
        paste0(workflow_name, ".yaml"),
        package = pkg_name
      )
      if (workflow_path == "") {
        stop("Workflow '", workflow_name, "' not found in package '", pkg_name, "'")
      }
      x <- workflow_path

      # Use package's mcp directory and ignore the tools_dir
      tools_dir <- system.file("mcp", "tools", package = pkg_name)
    }
  } else {
    source_path <- attr(x, "source_from")$path
    if(length(source_path) == 1 && !is.na(source_path) && dir.exists(source_path)) {
      if(!endsWith(x, ".yaml")) {
        workflow_path <- file.path(source_path, sprintf("%s.yaml", x))
      } else {
        workflow_path <- file.path(source_path, x)
      }
      if(file.exists(workflow_path)) {
        if(length(tools_dir) != 1 || is.na(tools_dir) ||
           !dir.exists(tools_dir) && file.exists(workflow_path)) {
          tools_dir <- file.path(dirname(source_path), "tools")
        }
        x <- workflow_path
      }
    }
  }
  # x is now a file path
  x <- normalizePath(x, mustWork = TRUE)
  if(is.null(tools_dir)) {
    tools_dir <- file.path(dirname(dirname(x)), "tools")
  }

  workflow <- tryCatch(
    {
      structure(
        yaml::read_yaml(x),
        class = c("ravepipeline_mcp_workflow", "list"),
        source_work_dir = dirname(dirname(x)),
        source_tools_dir = tools_dir
      )
    },
    error = function(e) {
      stop("Failed to parse workflow YAML at '", x, "': ", e$message, call. = FALSE)
    }
  )

  # Normalize mcp_tools field
  if(
    isTRUE(workflow$mcp_tools) ||
    identical(workflow$mcp_tools, "auto") ||
    identical(workflow$mcp_tools, "all")
  ) {
    # must load tools right now
    workflow$mcp_tools <- mcpflow_tool_names(workflow)
  } else if (isFALSE(workflow$mcp_tools)) {
    workflow$mcp_tools <- character(0L)
  }

  # Load tool instances if requested
  if (tools && length(workflow$mcp_tools) > 0) {
    attr(workflow, "mcp_tools") <- mcpflow_tools(workflow, tools_dir = tools_dir)
  }

  workflow
}

#' @export
mcpflow_read.ravepipeline_mcp_workflow <- function(x, tools = TRUE, tools_dir = NULL, ...) {
  # Already a workflow object
  # If tools=TRUE and not already loaded, try to load them
  if(is.null(tools_dir) || is.na(tools_dir)) {
    tools_dir <- attr(x, "source_tools_dir")
  }
  if(!length(tools_dir)) {
    tools_dir <- "./tools"
  }

  if(
    isTRUE(x$mcp_tools) ||
    identical(x$mcp_tools, "auto") ||
    identical(x$mcp_tools, "all")
  ) {
    x$mcp_tools <- mcpflow_tool_names(x)
  } else if (isFALSE(x$mcp_tools)) {
    x$mcp_tools <- character(0L)
  }

  if (tools && length(x$mcp_tools) && is.null(attr(x, "mcp_tools"))) {
    attr(x, "mcp_tools") <- mcpflow_tools(x, tools_dir = tools_dir)
  }

  x
}


#' Write RAVE MCP Workflow to File
#'
#' @description Writes a workflow object to YAML and/or Markdown format.
#'
#' @param workflow A ravepipeline_mcp_workflow object
#' @param path Character. Output file path. File extension determines format
#'   (.yaml for YAML, .md for Markdown). If no extension, writes both formats
#' @param method Character. Output format: "yaml" (default), "markdown", or "both"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the path(s) to written file(s)
#'
#' @examples
#' # Read a workflow
#' path <- system.file(
#'   "mcp", "workflows", "rave_pipeline_class_guide.yaml",
#'   package = "ravepipeline"
#' )
#' wf <- mcpflow_read(path)
#'
#' # Write as YAML to temporary file
#' yaml_file <- tempfile(fileext = ".yaml")
#' mcpflow_write(wf, yaml_file, method = "yaml")
#' cat(readLines(yaml_file), sep = "\n")
#'
#' # Write as Markdown to temporary file
#' md_file <- tempfile(fileext = ".md")
#' mcpflow_write(wf, md_file, method = "markdown")
#' cat(readLines(md_file), sep = "\n")
#'
#' # clean up
#' unlink(yaml_file)
#' unlink(md_file)
#'
#' @export
mcpflow_write <- function(workflow, path,
                          method = c("auto", "yaml", "markdown", "both"), ...) {

  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("path must be a non-empty character string")
  }

  workflow <- mcpflow_read(workflow, tools = FALSE)
  # Validate workflow first
  validation <- mcpflow_validate(workflow)
  if (!validation$valid) {
    warning("Workflow validation failed: ",
            paste(validation$errors, collapse = ", "),
            call. = FALSE
    )
  }

  method <- match.arg(method)

  # Determine output format from file extension if method not explicitly both
  if (method == "auto") {
    ext <- tolower(tools::file_ext(path))
    if (ext == "yaml" || ext == "yml") {
      method <- "yaml"
    } else if (ext == "md" || ext == "markdown") {
      method <- "markdown"
    } else {
      method <- "both"
    }
  }
  path <- tools::file_path_sans_ext(path)
  yaml_path <- sprintf("%s.yaml", path)
  md_path <- sprintf("%s.md", path)

  # Create parent directory if needed
  parent_dir <- dir_create2(dirname(path))

  written_files <- list()

  # Write YAML
  if (method %in% c("yaml", "both")) {
    # Remove S3 class before writing
    workflow_copy <- unclass(workflow)

    yaml::write_yaml(workflow_copy, yaml_path)
    written_files$yaml <- yaml_path
  }

  # Write Markdown
  if (method %in% c("markdown", "both")) {
    md_content <- convert_workflow_to_markdown(workflow)
    writeLines(md_content, md_path)
    written_files$markdown <- md_path
  }

  invisible(written_files)
}

#' Validate RAVE MCP Workflow
#'
#' @description Validates a workflow object for required fields and structure.
#'
#' @param workflow A ravepipeline_mcp_workflow object or list
#' @param available_tools Character vector of available tool names.
#'   If NULL, skips tool reference validation
#'
#' @return A list with components:
#'   \item{valid}{Logical, whether workflow is valid}
#'   \item{errors}{Character vector of error messages (empty if valid)}
#'   \item{warnings}{Character vector of warning messages}
#'
#' @examples
#' # Read a workflow
#' path <- system.file(
#'   "mcp", "workflows", "rave_pipeline_class_guide.yaml",
#'   package = "ravepipeline"
#' )
#' wf <- mcpflow_read(path)
#'
#' # Validate
#' result <- mcpflow_validate(wf)
#' result$valid
#'
#' @export
mcpflow_validate <- function(workflow, available_tools = NULL) {
  errors <- character(0)
  warnings <- character(0)

  # Check required fields
  if (is.null(workflow$name) || !nzchar(workflow$name)) {
    errors <- c(errors, "Missing required field: 'name'")
  }

  if (is.null(workflow$description)) {
    warnings <- c(warnings, "Missing recommended field: 'description'")
  }

  # Validate mcp_tools field
  if (!is.null(workflow$mcp_tools)) {
    if (!is.character(workflow$mcp_tools)) {
      errors <- c(errors, "'mcp_tools' must be character or NULL")
    } else if (length(workflow$mcp_tools) > 0) {
      # Check mcp_tools against available tools
      if (!is.null(available_tools)) {
        missing_tools <- setdiff(workflow$mcp_tools, available_tools)
        if (length(missing_tools) > 0) {
          warnings <- c(warnings, paste0(
            "mcp_tools field references unknown tools: ",
            paste(missing_tools, collapse = ", ")
          ))
        }
      }
    }
  }

  # Validate jobs structure
  if (!is.null(workflow$jobs)) {
    if (!is.list(workflow$jobs)) {
      errors <- c(errors, "'jobs' must be a list")
    } else {
      # Validate each job
      for (job_name in names(workflow$jobs)) {
        job <- workflow$jobs[[job_name]]

        if (!is.null(job$steps)) {
          if (!is.list(job$steps)) {
            errors <- c(errors, paste0("Job '", job_name, "': 'steps' must be a list"))
          } else {
            # Check tool references
            for (i in seq_along(job$steps)) {
              step <- job$steps[[i]]
              if (!is.null(step$tool)) {
                if (!is.null(available_tools) && !step$tool %in% available_tools) {
                  warnings <- c(warnings, paste0(
                    "Job '", job_name, "' step ", i,
                    " references unknown tool: '", step$tool, "'"
                  ))
                }
              }
            }
          }
        }
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}

#' Find Path to RAVE MCP Workflow
#'
#' @description Extracts the file path for a workflow by name from mcpflow_list results.
#'
#' @param workflow_name Character. Workflow name to find
#' @param pkg Character. Package name. If NULL, uses path parameter
#' @param path Character. Path to workflows directory.
#'   If NULL and pkg is provided, uses system.file("mcp", "workflows", package = pkg)
#'
#' @return Character string with the path to the workflow YAML file.
#'   Throws an error if workflow not found.
#'
#' @examples
#' # Find workflow path from package
#' wf_path <- mcpflow_path("rave_pipeline_class_guide", pkg = "ravepipeline")
#' wf_path
#'
#' @export
mcpflow_path <- function(workflow_name, pkg, path = system.file("mcp", "workflows", package = pkg)) {
  if (!is.character(workflow_name) || length(workflow_name) != 1 || !nzchar(workflow_name)) {
    stop("workflow_name must be a non-empty character string")
  }

  # Get workflow list with source info
  if(missing(pkg)) {
    workflows <- mcpflow_list(path = path)
  } else {
    workflows <- mcpflow_list(pkg = pkg, path = path)
  }
  source_info <- attr(workflows, "source_from")

  if (!workflow_name %in% workflows) {
    stop(
      "Workflow '", workflow_name, "' not found in ",
      if (!is.null(source_info$pkg)) paste0("package '", source_info$pkg, "'") else paste0("path '", source_info$path, "'")
    )
  }

  # Construct path to workflow file
  workflow_path <- file.path(source_info$path, paste0(workflow_name, ".yaml"))

  if (!file.exists(workflow_path)) {
    stop("Workflow file not found: '", workflow_path, "'")
  }

  workflow_path
}


#' Load RAVE MCP Workflows
#'
#' @description Loads workflow YAML files from a package's MCP workflows directory
#' and returns them as S3 objects with class "ravepipeline_mcp_workflow".
#'
#' @param pkg Character string. Package name to load workflows from.
#'   Example values: \code{"ravepipeline"}
#' @param path Character string. Optional path to workflows directory.
#'   If provided, loads from this path instead of package mcp/workflows.
#'
#' @return A named list of workflow objects, each with class
#'   c("ravepipeline_mcp_workflow", "list"). Returns empty list if no
#'   workflows found.
#'
#' @examples
#'
#' # MCP example response:
#' # {
#' #   "rave_pipeline_class_guide": {
#' #     "name": "rave_pipeline_class_guide",
#' #     "description": "...",
#' #     "mcp_tools": true
#' #   }
#' # }
#'
#' workflow <- mcpflow_load_all(pkg = "ravepipeline")
#'
#' @keywords mcp-tool mcp-category-discovery
#' @export
mcpflow_load_all <- function(pkg, path = system.file("mcp", package = pkg)) {
  # Determine workflow directory
  if(!nzchar(path)) {
    stop("Please specify a valid RAVE MCP workflow path")
  }
  path <- normalizePath(path, mustWork = TRUE)
  workflow_dir <- file.path(path, "workflows")
  tools_dir <- file.path(path, "tools")

  if (!dir.exists(workflow_dir)) {
    stop("No MCP workflows found in ", workflow_dir)
  }

  # Find all YAML files
  yaml_files <- list.files(workflow_dir, pattern = "\\.yaml$", full.names = TRUE)


  workflows <- lapply(yaml_files, function(x) {
    mcpflow_read(x, tools = TRUE, tools_dir = tools_dir)
  })

  # Remove NULL entries
  workflows <- workflows[!vapply(workflows, is.null, FALSE)]

  # Set names from workflow name field
  names(workflows) <- vapply(workflows, function(w) w$name %||% "", "")

  workflows
}


#' Export RAVE MCP Workflows
#'
#' @description Saves workflow objects to YAML and/or Markdown format.
#' Optionally exports associated MCP tools to the same directory.
#' Workflows are saved to output_dir/workflows/ and tools to output_dir/tools/.
#'
#' @param workflows Either a "ravepipeline_mcp_workflow" object, a list of
#'   "ravepipeline_mcp_workflow" objects, or a character string path to a workflow YAML file
#' @param output_dir Character string, base directory to save output files.
#'   If NULL (default), uses the parent directory of the input file's folder (when workflows is a path)
#'   or current working directory (when workflows is an object).
#'   Workflows are saved to output_dir/workflows/, tools to output_dir/tools/
#' @param format Character string, output format. Options: "markdown" (default),
#'   "yaml", "both"
#' @param save_tools Logical, whether to save referenced MCP tools to output_dir/tools/.
#'   Default is TRUE
#' @param verbose Logical, whether to print progress messages. Default is TRUE
#'
#' @return Invisibly returns a character vector of paths to written files
#'
#' @examples
#' # Load workflows from package and export to temp directory
#' workflows <- mcpflow_load_all("ravepipeline")
#'
#' temp_dir <- tempfile("mcp_export")
#' written_files <- mcpflow_export(
#'   workflows,
#'   output_dir = temp_dir,
#'   format = "both",
#'   save_tools = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Show exported files
#' list.files(temp_dir, recursive = TRUE)
#'
#' # Cleanup
#' unlink(temp_dir, recursive = TRUE)
#'
#' @export
mcpflow_export <- function(
    workflows,
    output_dir,
    format = c("yaml", "markdown", "both"),
    save_tools = TRUE,
    verbose = TRUE
) {

  format <- match.arg(format)
  force(output_dir)

  if(inherits(workflows, "ravepipeline_mcp_workflow")) {
    # single workflow
    workflows <- list(workflows)
  }

  workflows <- lapply(workflows, function(workflow) {
    # Force loading tools
    mcpflow_read(workflow, tools = save_tools)
  })

  workflows <- workflows[!vapply(workflows, is.null, FALSE)]

  # Create workflows subdirectory
  workflows_dir <- dir_create2(file.path(output_dir, "workflows"))
  tools_dir <- file.path(output_dir, "tools")
  if(save_tools) {
    tools_dir <- dir_create2(tools_dir)
  }

  lapply(workflows, function(workflow) {
    base_name <- gsub('[^a-zA-Z0-9_-]+', "-", workflow$name)
    if(verbose) {
      message("Saving MCP workflow: workflows/", base_name)
    }
    mcpflow_write(workflow, path = file.path(workflows_dir, base_name), method = format)
    if(save_tools) {
      tools <- mcpflow_tools(workflow)
      lapply(tools, function(tool) {
        tool_fname <- sprintf(
          "%s.yaml", gsub("[^a-zA-Z0-9_]+", "-", tool$name)
        )
        if(verbose) {
          message("Saving MCP tool: tools/", tool_fname)
        }
        mcptool_write(
          tool = tool,
          path = file.path(tools_dir, tool_fname),
          method = "yaml"
        )
        return()
      })
    }
    return()
  })

  invisible(output_dir)
}



#' Extract Tool Names from Workflows
#'
#' @description Recursively extracts all tool names referenced in workflow jobs and sections.
#' Supports both single workflow objects and lists of workflows.
#'
#' @param workflows A single workflow object (class "ravepipeline_mcp_workflow") or
#'   a list of workflow objects
#' @return Character vector of unique tool names
#'
#' @examples
#' # Extract tools from a single workflow
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#' tools <- mcpflow_tool_names(wf)
#'
#' # Extract tools from multiple workflows
#' workflows <- mcpflow_load_all("ravepipeline")
#' all_tools <- mcpflow_tool_names(workflows)
#' @export
mcpflow_tool_names <- function(workflows) {

  # Helper to recursively find tool references in any list structure
  find_tools_recursive <- function(x) {
    if (is.null(x)) return(character(0))

    found <- character(0)

    if (is.list(x)) {
      # Check for direct tool field
      if (!is.null(x$tool) && is.character(x$tool) && length(x$tool) == 1) {
        found <- c(found, x$tool)
      }

      # Recurse into all list elements
      for (item in x) {
        found <- c(found, find_tools_recursive(item))
      }
    }

    found
  }

  # Helper to extract tools from a single workflow
  extract_from_single <- function(workflow) {
    tools <- character(0)

    # Extract from jobs
    if (!is.null(workflow$jobs)) {
      tools <- c(tools, find_tools_recursive(workflow$jobs))
    }

    # Extract from sections
    if (!is.null(workflow$sections)) {
      tools <- c(tools, find_tools_recursive(workflow$sections))
    }

    tools
  }

  # Helper to check if an object looks like a single workflow
  # (has jobs or sections field, which are workflow-specific)
  is_single_workflow <- function(x) {
    inherits(x, "ravepipeline_mcp_workflow") ||
      (is.list(x) && !is.null(x$name) && (!is.null(x$jobs) || !is.null(x$sections)))
  }

  # Detect if workflows is a single workflow or a list of workflows
  if (is_single_workflow(workflows)) {
    # Single workflow
    tools <- extract_from_single(workflows)
  } else if (is.list(workflows)) {
    # List of workflows - extract from each
    tools <- unlist(lapply(workflows, function(w) {
      if (is_single_workflow(w)) {
        extract_from_single(w)
      } else {
        character(0)
      }
    }))
  } else {
    return(character(0))
  }

  unique(tools)
}


#' Load Tools for Workflow
#'
#'
#' @export
mcpflow_tools <- function(workflow, tools_dir = NULL) {
  if(is.null(tools_dir)) {
    tools_dir <- attr(workflow, "source_tools_dir")
  }
  if(!length(tools_dir)) {
    tools_dir <- "./tools"
  }
  if(isTRUE(workflow$mcp_tools) || isTRUE(workflow$mcp_tools %in% c("auto", "all"))) {
    tool_names <- mcpflow_tool_names(workflow)
  } else {
    if(isFALSE(workflow$mcp_tools)) {
      tool_names <- NULL
    } else {
      tool_names <- workflow$mcp_tools
    }
  }

  tools_list <- structure(
    names = tool_names,
    lapply(tool_names, function(tool_name) {
      tool_result <- mcptool_path(tool_name, tools_dir = tools_dir)
      tool_def <- attr(tool_result, "mcp_definition")
      tool_def
    })
  )

  tools_list <- tools_list[!vapply(tools_list, is.null, FALSE)]

  tools_list
}


#' Convert Workflow to Markdown
#'
#' @description Converts a workflow YAML object to formatted Markdown
#'
#' @param workflow A ravepipeline_mcp_workflow object
#'
#' @return Character vector of markdown lines
#'
#' @keywords internal
convert_workflow_to_markdown <- function(workflow) {
  lines <- character(0)

  # Header
  lines <- c(lines, paste0("# ", workflow$name %||% "Unnamed Workflow"))
  lines <- c(lines, "")

  # Description
  if (!is.null(workflow$description)) {
    lines <- c(lines, workflow$description)
    lines <- c(lines, "")
  }

  # Metadata
  metadata_start <- length(lines)
  if (!is.null(workflow$version)) {
    lines <- c(lines, paste0("**Version**: ", workflow$version))
  }
  if (!is.null(workflow$category)) {
    lines <- c(lines, paste0("**Category**: ", workflow$category))
  }
  if (!is.null(workflow$tags) && length(workflow$tags) > 0) {
    lines <- c(lines, paste0("**Tags**: ", paste(workflow$tags, collapse = ", ")))
  }
  # Add blank line only if metadata was added
  if (length(lines) > metadata_start) {
    lines <- c(lines, "")
  }

  # MCP Tools
  if (!is.null(workflow$mcp_tools) && length(workflow$mcp_tools) > 0) {
    lines <- c(lines, "## MCP Tools")
    lines <- c(lines, "")
    lines <- c(lines, "This workflow uses the following MCP tools:")
    lines <- c(lines, "")
    for (tool in workflow$mcp_tools) {
      lines <- c(lines, paste0("- `", tool, "`"))
    }
    lines <- c(lines, "")
  }

  # Settings
  if (!is.null(workflow$settings)) {
    lines <- c(lines, "## Settings")
    lines <- c(lines, "")

    settings <- workflow$settings
    if (!is.null(settings$dangerous) && settings$dangerous) {
      lines <- c(lines, "**WARNING**: This workflow includes dangerous operations.")
    }
    if (!is.null(settings$requires_approval) && settings$requires_approval) {
      lines <- c(lines, "**Requires user approval** before execution.")
    }
    if (!is.null(settings$estimated_duration)) {
      lines <- c(lines, paste0("**Estimated duration**: ", settings$estimated_duration))
    }
    lines <- c(lines, "")
  }

  # Sections (main content)
  if (!is.null(workflow$sections) && is.list(workflow$sections)) {
    for (section in workflow$sections) {
      if (!is.null(section$title)) {
        lines <- c(lines, paste0("## ", section$title))
        lines <- c(lines, "")
      }

      if (!is.null(section$content)) {
        if (is.character(section$content)) {
          lines <- c(lines, section$content)
          lines <- c(lines, "")
        } else if (is.list(section$content)) {
          # Handle structured content
          for (item in section$content) {
            if (is.character(item)) {
              lines <- c(lines, item)
            }
          }
          lines <- c(lines, "")
        }
      }

      # Workflows within sections
      if (!is.null(section$workflows) && is.list(section$workflows)) {
        for (wf in section$workflows) {
          if (!is.null(wf$name)) {
            lines <- c(lines, paste0("### ", wf$name))
            lines <- c(lines, "")
          }
          if (!is.null(wf$description)) {
            lines <- c(lines, wf$description)
            lines <- c(lines, "")
          }
        }
      }
    }
  }

  # Jobs
  if (!is.null(workflow$jobs) && is.list(workflow$jobs)) {
    lines <- c(lines, "## Workflow Jobs")
    lines <- c(lines, "")

    for (job_name in names(workflow$jobs)) {
      job <- workflow$jobs[[job_name]]

      lines <- c(lines, paste0("### ", job$name %||% job_name))
      lines <- c(lines, paste0("**Job ID**: `", job_name, "`"))
      lines <- c(lines, "")

      if (!is.null(job$description)) {
        lines <- c(lines, job$description)
        lines <- c(lines, "")
      }

      # Job dependencies (needs)
      if (!is.null(job$needs) && length(job$needs) > 0) {
        needs_list <- if (is.character(job$needs)) {
          paste0("`", job$needs, "`", collapse = ", ")
        } else {
          paste0("`", as.character(job$needs), "`", collapse = ", ")
        }
        lines <- c(lines, paste0("**Depends on**: ", needs_list))
        lines <- c(lines, "")
      }

      # Conditional execution (if)
      if (!is.null(job[["if"]]) && nzchar(job[["if"]])) {
        lines <- c(lines, paste0("**Condition**: `", job[["if"]], "`"))
        lines <- c(lines, "")
      }

      # Execution strategy
      if (!is.null(job$strategy) && is.list(job$strategy)) {
        strategy_parts <- character(0)
        if (isTRUE(job$strategy$parallel)) {
          strategy_parts <- c(strategy_parts, "parallel execution")
        }
        if (!is.null(job$strategy$max_concurrent)) {
          strategy_parts <- c(strategy_parts, paste0("max ", job$strategy$max_concurrent, " concurrent"))
        }
        if (length(strategy_parts) > 0) {
          lines <- c(lines, paste0("**Strategy**: ", paste(strategy_parts, collapse = ", ")))
          lines <- c(lines, "")
        }
      }

      if (!is.null(job$steps) && is.list(job$steps)) {
        lines <- c(lines, "**Steps:**")
        lines <- c(lines, "")

        for (i in seq_along(job$steps)) {
          step <- job$steps[[i]]
          step_name <- step$name %||% paste("Step", i)

          lines <- c(lines, paste0(i, ". **", step_name, "**"))

          if (!is.null(step$tool)) {
            lines <- c(lines, paste0("   - Tool: `", step$tool, "`"))
          }
          if (!is.null(step$description)) {
            lines <- c(lines, paste0("   - ", step$description))
          }

          # Step-level flags
          step_flags <- character(0)
          if (isTRUE(step$dangerous)) {
            step_flags <- c(step_flags, "DANGEROUS")
          }
          if (isTRUE(step$requires_approval)) {
            step_flags <- c(step_flags, "requires approval")
          }
          if (length(step_flags) > 0) {
            lines <- c(lines, paste0("   - **", paste(step_flags, collapse = ", "), "**"))
          }

          # Loop configuration
          if (!is.null(step$loop) && is.list(step$loop)) {
            loop_desc <- character(0)
            if (!is.null(step$loop$over)) {
              loop_desc <- c(loop_desc, paste0("over `", step$loop$over, "`"))
            }
            if (!is.null(step$loop$item)) {
              loop_desc <- c(loop_desc, paste0("as `", step$loop$item, "`"))
            }
            if (!is.null(step$loop$until)) {
              loop_desc <- c(loop_desc, paste0("until `", step$loop$until, "`"))
            }
            if (length(loop_desc) > 0) {
              lines <- c(lines, paste0("   - Loop: ", paste(loop_desc, collapse = " ")))
            }
          }

          # Validation rules
          if (!is.null(step$validation) && is.list(step$validation)) {
            if (!is.null(step$validation$check)) {
              lines <- c(lines, paste0("   - Validation: `", step$validation$check, "`"))
            }
            if (!is.null(step$validation$on_fail)) {
              lines <- c(lines, paste0("   - On failure: ", step$validation$on_fail))
            }
          }

          if (!is.null(step$with) && is.list(step$with)) {
            lines <- c(lines, "   - Parameters:")
            for (param_name in names(step$with)) {
              lines <- c(lines, paste0("     - `", param_name, "`: ", step$with[[param_name]]))
            }
          }
        }
        lines <- c(lines, "")
      }
    }
  }

  # Examples
  if (!is.null(workflow$examples) && is.list(workflow$examples)) {
    lines <- c(lines, "## Examples")
    lines <- c(lines, "")

    for (i in seq_along(workflow$examples)) {
      example <- workflow$examples[[i]]

      lines <- c(lines, paste0("### Example ", i))
      lines <- c(lines, "")

      if (!is.null(example$user_query)) {
        lines <- c(lines, paste0("**User Query**: ", example$user_query))
        lines <- c(lines, "")
      }

      if (!is.null(example$expected_flow)) {
        lines <- c(lines, paste0("**Expected Flow**: ", example$expected_flow))
        lines <- c(lines, "")
      }
    }
  }

  # Warnings
  if (!is.null(workflow$warnings) && length(workflow$warnings) > 0) {
    lines <- c(lines, "## Warnings")
    lines <- c(lines, "")
    for (warning_msg in workflow$warnings) {
      lines <- c(lines, paste0("- ", warning_msg))
    }
    lines <- c(lines, "")
  }

  # Best Practices
  if (!is.null(workflow$best_practices) && length(workflow$best_practices) > 0) {
    lines <- c(lines, "## Best Practices")
    lines <- c(lines, "")
    for (bp in workflow$best_practices) {
      if (is.character(bp)) {
        lines <- c(lines, paste0("- ", bp))
      } else if (is.list(bp)) {
        if (!is.null(bp$title)) {
          lines <- c(lines, paste0("### ", bp$title))
          lines <- c(lines, "")
        }
        if (!is.null(bp$content)) {
          lines <- c(lines, bp$content)
          lines <- c(lines, "")
        }
      }
    }
    lines <- c(lines, "")
  }

  lines
}


#' Print Method for RAVE MCP Workflows
#'
#' @param x A ravepipeline_mcp_workflow object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.ravepipeline_mcp_workflow <- function(x, ...) {
  cat("RAVE MCP Workflow:", x$name %||% "<unnamed>", "\n")

  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }

  if (!is.null(x$version)) {
    cat("Version:", x$version, "\n")
  }

  if (!is.null(x$category)) {
    cat("Category:", x$category, "\n")
  }

  # Tools
  if (!is.null(x$mcp_tools) && length(x$mcp_tools) > 0) {
    cat("MCP Tools:", length(x$mcp_tools), "\n")
  }

  # Jobs
  if (!is.null(x$jobs) && is.list(x$jobs)) {
    cat("Jobs:", length(x$jobs), "\n")
  }

  # Sections
  if (!is.null(x$sections) && is.list(x$sections)) {
    cat("Sections:", length(x$sections), "\n")
  }

  invisible(x)
}


#' Format Method for RAVE MCP Workflows
#'
#' @param x A ravepipeline_mcp_workflow object
#' @param ... Additional arguments (ignored)
#'
#' @return Character string representation
#'
#' @export
format.ravepipeline_mcp_workflow <- function(x, ...) {
  paste0("<RAVE MCP Workflow: ", x$name %||% "<unnamed>", ">")
}


#' Create Job Execution Validator for Workflow
#'
#' @description Creates a callback function that validates tool calls against
#' workflow job definitions. The validator tracks which jobs have been completed
#' and warns (or rejects) when tools are called out of expected order or
#' when job dependencies are not met.
#'
#' @param workflow A \code{ravepipeline_mcp_workflow} object
#' @param strict Logical. If \code{TRUE}, reject out-of-order tool calls via
#'   \code{ellmer::tool_reject()}. If \code{FALSE} (default), only warn via
#'   \code{cli::cli_warn()}.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages
#'   when jobs and steps are detected.
#'
#' @return A list with two callback functions:
#'   \item{on_tool_request}{Callback for \code{chat$on_tool_request()}}
#'   \item{on_tool_result}{Callback for \code{chat$on_tool_result()}}
#'   \item{state}{An environment containing execution state (for inspection)}
#'
#' @details
#' The validator maintains state about which jobs have been started/completed
#' based on tool calls observed. It checks:
#' \itemize{
#'   \item Whether the tool being called matches expected job steps
#'   \item Whether job dependencies (\code{needs}) are satisfied
#'   \item Step execution order within jobs
#' }
#'
#' @examples
#' \dontrun{
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#' validator <- mcpflow_job_validator(wf, strict = FALSE)
#'
#' chat <- ellmer::chat_ollama()
#' chat$on_tool_request(validator$on_tool_request)
#' chat$on_tool_result(validator$on_tool_result)
#' }
#'
#' @export
mcpflow_job_validator <- function(workflow, strict = FALSE, verbose = TRUE) {
  workflow <- mcpflow_read(workflow, tools = FALSE)

  # Extract job structure for validation
  jobs <- workflow$jobs %||% list()
  job_names <- names(jobs)

  # Build dependency graph and tool->job mapping
  job_deps <- list()       # job_name -> character vector of dependencies

  tool_to_jobs <- list()   # tool_name -> list of (job_name, step_index)

  for (job_name in job_names) {
    job <- jobs[[job_name]]
    job_deps[[job_name]] <- job$needs %||% character(0)

    if (!is.null(job$steps) && is.list(job$steps)) {
      for (i in seq_along(job$steps)) {
        step <- job$steps[[i]]
        if (!is.null(step$tool) && is.character(step$tool)) {
          tool_name <- step$tool
          if (is.null(tool_to_jobs[[tool_name]])) {
            tool_to_jobs[[tool_name]] <- list()
          }
          tool_to_jobs[[tool_name]] <- c(tool_to_jobs[[tool_name]], list(list(
            job = job_name,
            step = i,
            step_name = step$name %||% paste("Step", i),
            dangerous = isTRUE(step$dangerous),
            requires_approval = isTRUE(step$requires_approval)
          )))
        }
      }
    }
  }

  # Execution state (mutable environment)
  state <- new.env(parent = emptyenv())
  state$completed_jobs <- character(0)
  state$current_job <- NULL
  state$current_step <- 0
  state$tool_call_history <- list()

  # Helper: check if job dependencies are satisfied
  check_deps_satisfied <- function(job_name) {
    deps <- job_deps[[job_name]]
    if (length(deps) == 0) return(TRUE)
    all(deps %in% state$completed_jobs)
  }

  # on_tool_request callback
  on_tool_request <- function(request) {
    tool_name <- request@name

    # Record in history
    state$tool_call_history <- c(state$tool_call_history, list(list(
      tool = tool_name,
      arguments = request@arguments,
      timestamp = Sys.time()
    )))

    # Check if this tool is part of any job
    job_refs <- tool_to_jobs[[tool_name]]
    if (is.null(job_refs) || length(job_refs) == 0) {
      # Tool not in any job - allow it (could be utility call)
      return(invisible(NULL))
    }

    # Find the most likely job/step this call corresponds to
    # Prefer continuing current job, then check for new jobs with satisfied deps
    matched <- NULL

    for (ref in job_refs) {
      if (!is.null(state$current_job) && ref$job == state$current_job) {
        # Continuing current job
        if (ref$step == state$current_step + 1) {
          matched <- ref
          break
        }
      }
    }

    if (is.null(matched)) {
      # Try to find a job with satisfied dependencies
      for (ref in job_refs) {
        if (check_deps_satisfied(ref$job)) {
          matched <- ref
          break
        }
      }
    }

    if (is.null(matched)) {
      # Dependencies not satisfied
      msg <- sprintf(
        "Tool '%s' is part of job(s) with unsatisfied dependencies. Expected jobs to complete first: %s",
        tool_name,
        paste(unique(vapply(job_refs, function(r) {
          paste(job_deps[[r$job]], collapse = ", ")
        }, "")), collapse = "; ")
      )
      if (strict) {
        ellmer <- asNamespace("ellmer")
        ellmer$tool_reject(msg)
      } else if (verbose) {
        cli::cli_warn(msg)
      }
      return(invisible(NULL))
    }

    # Update state
    if (is.null(state$current_job) || matched$job != state$current_job) {
      # Starting a new job
      if (!is.null(state$current_job) && verbose) {
        cli::cli_inform("Workflow: transitioning from job '{state$current_job}' to '{matched$job}'")
      }
      state$current_job <- matched$job
      state$current_step <- 0
    }

    # Check step order
    if (matched$step != state$current_step + 1 && verbose) {
      cli::cli_warn(
        "Workflow: expected step {state$current_step + 1} in job '{matched$job}', but tool suggests step {matched$step}"
      )
    }

    state$current_step <- matched$step

    if (verbose) {
      cli::cli_inform("Workflow: job '{matched$job}' step {matched$step}: {matched$step_name}")
    }

    # Warn about dangerous/approval flags

    if (matched$dangerous && verbose) {
      cli::cli_alert_danger("This step is marked as DANGEROUS")
    }
    if (matched$requires_approval && verbose) {
      cli::cli_alert_warning("This step requires user approval before execution")
    }

    invisible(NULL)
  }

  # on_tool_result callback
  on_tool_result <- function(result) {
    # Check if current job is complete (all steps done)
    if (!is.null(state$current_job)) {
      job <- jobs[[state$current_job]]
      n_steps <- length(job$steps %||% list())

      if (state$current_step >= n_steps) {
        # Job complete
        state$completed_jobs <- c(state$completed_jobs, state$current_job)
        if (verbose) {
          cli::cli_inform("Workflow: job '{state$current_job}' completed")
        }
        state$current_job <- NULL
        state$current_step <- 0
      }
    }

    invisible(NULL)
  }

  list(
    on_tool_request = on_tool_request,
    on_tool_result = on_tool_result,
    state = state
  )
}


#' Instantiate Workflow as ellmer Chat
#'
#' @description Creates an \code{ellmer} chat object configured with the workflow's
#' system prompt (derived from workflow content) and all referenced tools registered.
#' Optionally attaches validation callbacks to track job execution.
#'
#' @param workflow A \code{ravepipeline_mcp_workflow} object, or a path/identifier
#'   that can be read via \code{\link{mcpflow_read}}.
#' @param chat An existing \code{ellmer} chat object to configure. If \code{NULL}
#'   (default), a new chat is created using \code{chat_provider}.
#' @param chat_provider Character. The chat model to use when creating a new chat.
#'   One of \code{"ollama"} (default), \code{"openai"}, \code{"claude"},
#'   \code{"gemini"}, \code{"cortex"}, \code{"azure_openai"}, \code{"bedrock"},
#'   \code{"databricks"}, \code{"github"}, \code{"groq"}, \code{"perplexity"},
#'   \code{"snowflake"}, \code{"vllm"}. Only used when \code{chat} is \code{NULL}.
#' @param chat_args A named list of additional arguments passed to the
#'   \code{ellmer::chat_*} constructor (e.g., \code{model}, \code{api_key},
#'   \code{base_url}). Only used when \code{chat} is \code{NULL}.
#' @param on_tool_request Optional callback function for tool request events.
#'   Passed to \code{chat$on_tool_request()}. Receives a \code{ContentToolRequest}
#'   object. Can call \code{ellmer::tool_reject(reason)} to prevent execution.
#' @param on_tool_result Optional callback function for tool result events.
#'   Passed to \code{chat$on_tool_result()}. Receives a \code{ContentToolResult}
#'   object.
#' @param use_job_validator Logical. If \code{TRUE}, automatically attach a
#'   job validator created by \code{\link{mcpflow_job_validator}}. Default is
#'   \code{FALSE}. Ignored if \code{on_tool_request} or \code{on_tool_result}
#'   are provided.
#' @param validator_strict Logical. If \code{TRUE} and \code{use_job_validator}
#'   is \code{TRUE}, the validator will reject out-of-order tool calls. Default
#'   is \code{FALSE} (advisory warnings only).
#' @param validator_verbose Logical. If \code{TRUE} (default) and
#'   \code{use_job_validator} is \code{TRUE}, print progress messages.
#' @param ... Additional arguments passed to \code{\link{mcptool_instantiate}}
#'   for each tool.
#'
#' @return An \code{ellmer} chat object with:
#'   \itemize{
#'     \item System prompt set from workflow content (via \code{convert_workflow_to_markdown})
#'     \item All referenced MCP tools registered
#'     \item Optional validation callbacks attached
#'   }
#'
#' @details
#' The system prompt is generated by \code{\link{convert_workflow_to_markdown}},
#' which includes:
#' \itemize{
#'   \item Workflow name and description
#'   \item Tool list
#'   \item Settings (dangerous, requires_approval, estimated_duration)
#'   \item Sections with content
#'   \item Jobs with dependencies, conditions, and step details
#'   \item Examples, warnings, and best practices
#' }
#'
#' @examples
#' \dontrun{
#' # Load workflow and create chat with Ollama
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#' chat <- mcpflow_instantiate(wf)
#'
#' # Create chat with OpenAI and custom model
#' chat <- mcpflow_instantiate(
#'   wf,
#'   chat_provider = "openai",
#'   chat_args = list(model = "gpt-4")
#' )
#'
#' # Use existing chat object
#' existing_chat <- ellmer::chat_claude()
#' chat <- mcpflow_instantiate(wf, chat = existing_chat)
#'
#' # Enable job validation (advisory mode)
#' chat <- mcpflow_instantiate(wf, use_job_validator = TRUE)
#'
#' # Enable strict job validation
#' chat <- mcpflow_instantiate(wf, use_job_validator = TRUE, validator_strict = TRUE)
#'
#' # Use chat
#' chat$chat("Help me set up a power analysis pipeline")
#' }
#'
#' @export
mcpflow_instantiate <- function(
    workflow,
    chat = NULL,
    chat_provider = c("ollama", "openai", "claude", "gemini", "cortex",
                   "azure_openai", "bedrock", "databricks", "github",
                   "groq", "perplexity", "snowflake", "vllm"),
    chat_args = list(),
    on_tool_request = NULL,
    on_tool_result = NULL,
    use_job_validator = FALSE,
    validator_strict = FALSE,
    validator_verbose = TRUE,
    ...
) {
  # Ensure workflow is loaded with tools
  workflow <- mcpflow_read(workflow, tools = TRUE)

  # Check ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required to instantiate workflow chat.")
  }
  ellmer <- asNamespace("ellmer")

  # Create or validate chat object
  if (is.null(chat)) {
    chat_provider <- match.arg(chat_provider)
    chat_constructor <- ellmer[[sprintf("chat_%s", chat_provider)]]

    if (is.null(chat_constructor) || !is.function(chat_constructor)) {
      stop("ellmer does not have a 'chat_", chat_provider, "' function.")
    }

    chat <- do.call(chat_constructor, chat_args)
  }

  # Generate system prompt from workflow
  system_prompt <- paste(convert_workflow_to_markdown(workflow), collapse = "\n")

  # Set system prompt
  # ellmer chat objects store system prompt - try common methods
  if (is.function(chat$set_system_prompt)) {
    chat$set_system_prompt(system_prompt)
  } else {
    # For ellmer Chat R6 class, system_prompt is typically set at construction
    # or via a method. Try direct assignment if available
    if ("system_prompt" %in% names(chat)) {
      chat$system_prompt <- system_prompt
    } else {
      warning(
        "Could not set system prompt on chat object. ",
        "Consider passing system_prompt via chat_args.",
        call. = FALSE
      )
    }
  }

  # Register tools
  tools <- attr(workflow, "mcp_tools")
  if (!is.null(tools) && length(tools) > 0) {
    for (tool in tools) {
      if (!is.null(tool)) {
        ellmer_tool <- mcptool_instantiate(tool, ...)
        chat$register_tool(ellmer_tool)
      }
    }
  }

  # Set up job validator if requested and no custom callbacks provided
  if (use_job_validator && is.null(on_tool_request) && is.null(on_tool_result)) {
    validator <- mcpflow_job_validator(
      workflow,
      strict = validator_strict,
      verbose = validator_verbose
    )
    on_tool_request <- validator$on_tool_request
    on_tool_result <- validator$on_tool_result
  }

  # Attach callbacks
  if (is.function(on_tool_request)) {
    chat$on_tool_request(on_tool_request)
  }
  if (is.function(on_tool_result)) {
    chat$on_tool_result(on_tool_result)
  }

  chat
}
