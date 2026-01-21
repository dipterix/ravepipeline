# RAVE MCP Workflows System
# Workflow loading, saving, validation, and S3 class definitions


# -- Internal YAML Schema Validation

#' Validate Workflow `YAML` Schema (Internal)
#'
#' @description Strictly validates a workflow `YAML` file or parsed `YAML` list
#' against the expected schema. This is an internal function used to ensure
#' `YAML` files conform to the agreed-upon structure.
#'
#' @param x Either a file path to a `YAML` file, or a list read from the file
#'
#' @return A list with components:
#'   \item{`valid`}{Logical, whether the YAML is valid}
#'
#'   \item{`errors`}{Character vector of error messages (schema violations)}
#'   \item{`warnings`}{Character vector of warning messages (recommendations)}
#'
#' @keywords internal
mcpflow_validate_yaml <- function(x) {
  errors <- character(0)
  warnings <- character(0)


  # Helper to add error with path context
  add_error <- function(path, msg) {
    errors <<- c(errors, sprintf("[%s] %s", path, msg))
  }

  # Helper to add warning with path context
  add_warning <- function(path, msg) {
    warnings <<- c(warnings, sprintf("[%s] %s", path, msg))
  }

  # Helper to check type
  check_type <- function(value, expected, path, required = FALSE) {
    if (is.null(value)) {
      if (required) {
        add_error(path, sprintf("Required field is missing"))
      }
      return(FALSE)
    }

    valid <- switch(expected,
      "string" = is.character(value) && length(value) == 1,
      "string_or_null" = is.null(value) || (is.character(value) && length(value) == 1),
      "character" = is.character(value),
      "logical" = is.logical(value) && length(value) == 1,
      "logical_or_string" = (is.logical(value) && length(value) == 1) ||
                            (is.character(value) && length(value) == 1),
      "list" = is.list(value),
      "named_list" = is.list(value) && !is.null(names(value)) && all(nzchar(names(value))),
      "integer" = is.numeric(value) && length(value) == 1 && value == as.integer(value),
      TRUE
    )

    if (!valid) {
      add_error(path, sprintf("Expected type '%s', got '%s'", expected, class(value)[1]))
    }
    valid
  }

  # Read YAML if path provided
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    yaml_data <- tryCatch(
      yaml::read_yaml(x),
      error = function(e) {
        add_error("ROOT", sprintf("Failed to parse YAML: %s", e$message))
        NULL
      }
    )
    if (is.null(yaml_data)) {
      return(list(valid = FALSE, errors = errors, warnings = warnings))
    }
  } else if (is.list(x)) {
    yaml_data <- x
  } else {
    add_error("ROOT", "Input must be a file path or a list from yaml::read_yaml")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # ---- METADATA ----
  check_type(yaml_data$name, "string", "name", required = TRUE)

  if (!is.null(yaml_data$description)) {
    check_type(yaml_data$description, "string", "description")
  }

  if (!is.null(yaml_data$version)) {
    check_type(yaml_data$version, "string", "version")
  }

  if (!is.null(yaml_data$category)) {
    check_type(yaml_data$category, "string", "category")
  }

  if (!is.null(yaml_data$tags)) {
    check_type(yaml_data$tags, "character", "tags")
  }

  # ---- MCP_TOOLS ----
  if (!is.null(yaml_data$mcp_tools)) {
    if (!isTRUE(yaml_data$mcp_tools) && !isFALSE(yaml_data$mcp_tools)) {
      if (!is.character(yaml_data$mcp_tools)) {
        add_error("mcp_tools", "Must be TRUE, FALSE, or a character vector of tool names")
      } else if (length(yaml_data$mcp_tools) == 1) {
        if (!yaml_data$mcp_tools %in% c("auto", "all")) {
          add_error("mcp_tools", "Single string value must be 'auto' or 'all'")
        }
      }
    }
  }

  # ---- TOOL_GUIDE ----
  if (!is.null(yaml_data$tool_guide)) {
    if (!is.list(yaml_data$tool_guide)) {
      add_error("tool_guide", "Must be a list")
    } else {
      for (i in seq_along(yaml_data$tool_guide)) {
        tg <- yaml_data$tool_guide[[i]]
        path_prefix <- sprintf("tool_guide[%d]", i)

        if (!is.list(tg)) {
          add_error(path_prefix, "Each tool_guide entry must be a list")
          next
        }

        check_type(tg$tool, "string", sprintf("%s.tool", path_prefix), required = TRUE)

        if (!is.null(tg$category)) {
          check_type(tg$category, "string", sprintf("%s.category", path_prefix))
        }
        if (!is.null(tg$when)) {
          check_type(tg$when, "string", sprintf("%s.when", path_prefix))
        }
        if (!is.null(tg$notes)) {
          check_type(tg$notes, "string", sprintf("%s.notes", path_prefix))
        }
        if (!is.null(tg$dangerous)) {
          check_type(tg$dangerous, "logical", sprintf("%s.dangerous", path_prefix))
        }
        if (!is.null(tg$requires_approval)) {
          check_type(tg$requires_approval, "logical", sprintf("%s.requires_approval", path_prefix))
        }
        if (!is.null(tg$preconditions)) {
          check_type(tg$preconditions, "character", sprintf("%s.preconditions", path_prefix))
        }

        # Check for unknown fields in tool_guide entry
        known_tg_fields <- c("tool", "category", "when", "notes", "dangerous",
                             "requires_approval", "preconditions", "examples")
        unknown_tg <- setdiff(names(tg), known_tg_fields)
        if (length(unknown_tg) > 0) {
          add_error(path_prefix, sprintf("Unknown field(s): %s", paste(unknown_tg, collapse = ", ")))
        }
      }
    }
  }

  # ---- OVERVIEW ----
  if (!is.null(yaml_data$overview)) {
    check_type(yaml_data$overview, "string", "overview")
  }

  # ---- BEST_PRACTICES ----
  if (!is.null(yaml_data$best_practices)) {
    if (!is.list(yaml_data$best_practices)) {
      add_error("best_practices", "Must be a list")
    } else {
      for (i in seq_along(yaml_data$best_practices)) {
        bp <- yaml_data$best_practices[[i]]
        path_prefix <- sprintf("best_practices[%d]", i)

        if (!is.list(bp)) {
          add_error(path_prefix, "Each best_practices entry must be a list")
          next
        }

        check_type(bp$title, "string", sprintf("%s.title", path_prefix), required = TRUE)
        check_type(bp$do, "string", sprintf("%s.do", path_prefix), required = TRUE)
        check_type(bp$dont, "string", sprintf("%s.dont", path_prefix), required = TRUE)

        # Check for unknown fields
        known_bp_fields <- c("title", "do", "dont")
        unknown_bp <- setdiff(names(bp), known_bp_fields)
        if (length(unknown_bp) > 0) {
          add_error(path_prefix, sprintf("Unknown field(s): %s", paste(unknown_bp, collapse = ", ")))
        }
      }
    }
  }

  # ---- WARNINGS ----
  if (!is.null(yaml_data$warnings)) {
    check_type(yaml_data$warnings, "character", "warnings")
  }

  # ---- JOBS ----
  if (!is.null(yaml_data$jobs)) {
    if (!check_type(yaml_data$jobs, "named_list", "jobs")) {
      # Skip job validation if not a named list
    } else {
      for (job_name in names(yaml_data$jobs)) {
        job <- yaml_data$jobs[[job_name]]
        job_path <- sprintf("jobs.%s", job_name)

        if (!is.list(job)) {
          add_error(job_path, "Job must be a list")
          next
        }

        # Job fields
        if (!is.null(job$name)) {
          check_type(job$name, "string", sprintf("%s.name", job_path))
        }
        if (!is.null(job$description)) {
          check_type(job$description, "string", sprintf("%s.description", job_path))
        }
        if (!is.null(job[["if"]])) {
          check_type(job[["if"]], "string", sprintf("%s.if", job_path))
        }

        # Strategy
        if (!is.null(job$strategy)) {
          strat_path <- sprintf("%s.strategy", job_path)
          if (!check_type(job$strategy, "list", strat_path)) {
            # Skip
          } else {
            if (!is.null(job$strategy$parallel)) {
              check_type(job$strategy$parallel, "logical", sprintf("%s.parallel", strat_path))
            }
            if (!is.null(job$strategy$max_concurrent)) {
              check_type(job$strategy$max_concurrent, "integer", sprintf("%s.max_concurrent", strat_path))
            }

            known_strat_fields <- c("parallel", "max_concurrent")
            unknown_strat <- setdiff(names(job$strategy), known_strat_fields)
            if (length(unknown_strat) > 0) {
              add_error(strat_path, sprintf("Unknown field(s): %s", paste(unknown_strat, collapse = ", ")))
            }
          }
        }

        # Steps
        if (!is.null(job$steps)) {
          if (!is.list(job$steps)) {
            add_error(sprintf("%s.steps", job_path), "Must be a list")
          } else {
            for (si in seq_along(job$steps)) {
              step <- job$steps[[si]]
              step_path <- sprintf("%s.steps[%d]", job_path, si)

              if (!is.list(step)) {
                add_error(step_path, "Step must be a list")
                next
              }

              # Must have either tool or action
              has_tool <- !is.null(step$tool)
              has_action <- !is.null(step$action)

              if (!has_tool && !has_action) {
                add_error(step_path, "Step must have either 'tool' or 'action'")
              }

              if (has_tool) {
                check_type(step$tool, "string", sprintf("%s.tool", step_path))
              }
              if (has_action) {
                check_type(step$action, "string", sprintf("%s.action", step_path))
              }
              if (!is.null(step$name)) {
                check_type(step$name, "string", sprintf("%s.name", step_path))
              }
              if (!is.null(step$description)) {
                check_type(step$description, "string", sprintf("%s.description", step_path))
              }
              if (!is.null(step$dangerous)) {
                check_type(step$dangerous, "logical", sprintf("%s.dangerous", step_path))
              }
              if (!is.null(step$requires_approval)) {
                check_type(step$requires_approval, "logical", sprintf("%s.requires_approval", step_path))
              }

              # with
              if (!is.null(step$with)) {
                check_type(step$with, "named_list", sprintf("%s.with", step_path))
              }

              # loop
              if (!is.null(step$loop)) {
                loop_path <- sprintf("%s.loop", step_path)
                if (!check_type(step$loop, "list", loop_path)) {
                  # Skip
                } else {
                  if (!is.null(step$loop$over)) {
                    check_type(step$loop$over, "string", sprintf("%s.over", loop_path))
                  }
                  if (!is.null(step$loop$item)) {
                    check_type(step$loop$item, "string", sprintf("%s.item", loop_path))
                  }
                  if (!is.null(step$loop$until)) {
                    check_type(step$loop$until, "string", sprintf("%s.until", loop_path))
                  }
                  if (!is.null(step$loop$interval)) {
                    check_type(step$loop$interval, "string", sprintf("%s.interval", loop_path))
                  }

                  known_loop_fields <- c("over", "item", "until", "interval")
                  unknown_loop <- setdiff(names(step$loop), known_loop_fields)
                  if (length(unknown_loop) > 0) {
                    add_error(loop_path, sprintf("Unknown field(s): %s", paste(unknown_loop, collapse = ", ")))
                  }
                }
              }

              # validation
              if (!is.null(step$validation)) {
                val_path <- sprintf("%s.validation", step_path)
                if (!check_type(step$validation, "list", val_path)) {
                  # Skip
                } else {
                  if (!is.null(step$validation$check)) {
                    check_type(step$validation$check, "string", sprintf("%s.check", val_path))
                  }
                  if (!is.null(step$validation$on_fail)) {
                    check_type(step$validation$on_fail, "string", sprintf("%s.on_fail", val_path))
                  }

                  known_val_fields <- c("check", "on_fail")
                  unknown_val <- setdiff(names(step$validation), known_val_fields)
                  if (length(unknown_val) > 0) {
                    add_error(val_path, sprintf("Unknown field(s): %s", paste(unknown_val, collapse = ", ")))
                  }
                }
              }

              # Check for unknown step fields
              known_step_fields <- c("name", "tool", "action", "description", "with",
                                     "loop", "validation", "dangerous", "requires_approval")
              unknown_step <- setdiff(names(step), known_step_fields)
              if (length(unknown_step) > 0) {
                add_error(step_path, sprintf("Unknown field(s): %s", paste(unknown_step, collapse = ", ")))
              }
            }
          }
        }

        # Check for unknown job fields
        known_job_fields <- c("name", "description", "if", "strategy", "steps")
        unknown_job <- setdiff(names(job), known_job_fields)
        if (length(unknown_job) > 0) {
          add_error(job_path, sprintf("Unknown field(s): %s", paste(unknown_job, collapse = ", ")))
        }
      }
    }
  }

  # ---- EXAMPLES ----
  if (!is.null(yaml_data$examples)) {
    if (!is.list(yaml_data$examples)) {
      add_error("examples", "Must be a list")
    } else {
      for (i in seq_along(yaml_data$examples)) {
        ex <- yaml_data$examples[[i]]
        ex_path <- sprintf("examples[%d]", i)

        if (!is.list(ex)) {
          add_error(ex_path, "Each example must be a list")
          next
        }

        check_type(ex$trigger, "string", sprintf("%s.trigger", ex_path), required = TRUE)

        if (is.null(ex$flow)) {
          add_error(sprintf("%s.flow", ex_path), "Required field is missing")
        } else if (!is.list(ex$flow)) {
          add_error(sprintf("%s.flow", ex_path), "Must be a list")
        } else {
          for (fi in seq_along(ex$flow)) {
            fl <- ex$flow[[fi]]
            fl_path <- sprintf("%s.flow[%d]", ex_path, fi)

            if (!is.list(fl)) {
              add_error(fl_path, "Each flow entry must be a list")
              next
            }

            # Must have either tool or action
            has_tool <- !is.null(fl$tool)
            has_action <- !is.null(fl$action)

            if (!has_tool && !has_action) {
              add_error(fl_path, "Flow entry must have either 'tool' or 'action'")
            }

            if (has_tool) {
              check_type(fl$tool, "string", sprintf("%s.tool", fl_path))
            }
            if (has_action) {
              check_type(fl$action, "string", sprintf("%s.action", fl_path))
            }
            if (!is.null(fl$says)) {
              check_type(fl$says, "string", sprintf("%s.says", fl_path))
            }
            if (!is.null(fl$with)) {
              check_type(fl$with, "named_list", sprintf("%s.with", fl_path))
            }

            known_flow_fields <- c("tool", "action", "says", "with")
            unknown_flow <- setdiff(names(fl), known_flow_fields)
            if (length(unknown_flow) > 0) {
              add_error(fl_path, sprintf("Unknown field(s): %s", paste(unknown_flow, collapse = ", ")))
            }
          }
        }

        known_ex_fields <- c("trigger", "flow")
        unknown_ex <- setdiff(names(ex), known_ex_fields)
        if (length(unknown_ex) > 0) {
          add_error(ex_path, sprintf("Unknown field(s): %s", paste(unknown_ex, collapse = ", ")))
        }
      }
    }
  }

  # ---- SETTINGS ----
  if (!is.null(yaml_data$settings)) {
    if (!check_type(yaml_data$settings, "list", "settings")) {
      # Skip
    } else {
      if (!is.null(yaml_data$settings$dangerous)) {
        check_type(yaml_data$settings$dangerous, "logical", "settings.dangerous")
      }
      if (!is.null(yaml_data$settings$requires_approval)) {
        check_type(yaml_data$settings$requires_approval, "logical", "settings.requires_approval")
      }
      if (!is.null(yaml_data$settings$estimated_duration)) {
        check_type(yaml_data$settings$estimated_duration, "string", "settings.estimated_duration")
      }

      known_settings_fields <- c("dangerous", "requires_approval", "estimated_duration")
      unknown_settings <- setdiff(names(yaml_data$settings), known_settings_fields)
      if (length(unknown_settings) > 0) {
        add_error("settings", sprintf("Unknown field(s): %s", paste(unknown_settings, collapse = ", ")))
      }
    }
  }

  # ---- PROMPT_SECTIONS ----
  # Valid section names for prompt splitting
  valid_section_names <- c("overview", "tool_guide", "warnings", "coding_guidelines",
                           "code_generation_rules", "best_practices", "jobs", "examples")

  if (!is.null(yaml_data$prompt_sections)) {
    if (!check_type(yaml_data$prompt_sections, "list", "prompt_sections")) {
      # Skip
    } else {
      # prompt_sections.core is required and must be a character vector
      if (is.null(yaml_data$prompt_sections$core)) {
        add_error("prompt_sections", "Missing required field 'core'. Must specify which sections are included in the core system prompt.")
      } else if (!is.character(yaml_data$prompt_sections$core)) {
        add_error("prompt_sections.core", "Must be a character vector of section names")
      } else {
        # Validate that all specified sections exist
        invalid_sections <- setdiff(yaml_data$prompt_sections$core, valid_section_names)
        if (length(invalid_sections) > 0) {
          add_error("prompt_sections.core",
                    sprintf("Invalid section name(s): %s. Valid sections are: %s",
                            paste(invalid_sections, collapse = ", "),
                            paste(valid_section_names, collapse = ", ")))
        }
      }

      known_ps_fields <- c("core")
      unknown_ps <- setdiff(names(yaml_data$prompt_sections), known_ps_fields)
      if (length(unknown_ps) > 0) {
        add_error("prompt_sections", sprintf("Unknown field(s): %s", paste(unknown_ps, collapse = ", ")))
      }
    }
  }

  # ---- CHECK FOR UNKNOWN TOP-LEVEL FIELDS ----
  known_top_fields <- c("name", "description", "version", "category", "tags",
                        "mcp_tools", "tool_guide", "overview", "best_practices",
                        "warnings", "jobs", "examples", "settings", "prompt_sections",
                        "coding_guidelines", "code_generation_rules")
  unknown_top <- setdiff(names(yaml_data), known_top_fields)
  if (length(unknown_top) > 0) {
    add_error("ROOT", sprintf("Unknown top-level field(s): %s", paste(unknown_top, collapse = ", ")))
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}


#' List RAVE MCP workflow names
#'
#' @description Lists all available MCP workflow names from a package or
#' directory. Returns workflow names with source information (path and
#' package) as attributes.
#'
#' @param pkg Character. Package name. If missing, must specify path directly
#' @param path Character. Path to workflows directory.
#'   Default is \code{system.file("mcp", "workflows", package = pkg)}
#'
#' @return Character vector of workflow names with attribute \code{"source_from"}
#'   containing a list with \code{"path"} and \code{"pkg"} (if applicable)
#'
#' @examples
#'
#'
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

#' Read RAVE MCP workflow
#'
#' @description Reads and parses a single MCP workflow from various sources.
#'
#' @param x Source specification: character path, \code{"package::name"},
#'   package namespace (environment), or workflow object
#' @param tools Logical. Whether to load referenced MCP tools. Default is TRUE
#' @param tools_dir Character. Base directory containing `tools/` directory
#'  for loading local tools. Default is NULL, which infers from the workflow
#'  file location or falls back to "./tools".
#' @param ... Additional arguments passed to methods
#'
#' @return A workflow object with class \code{"ravepipeline_mcp_workflow"}
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


#' Write RAVE MCP workflow to a file
#'
#' @description Writes a workflow object to `YAML` and/or `Markdown` format.
#'
#' @param workflow A \code{'ravepipeline_mcp_workflow'} object
#' @param path Character. Output file path. File extension determines format
#'   (`.yaml` for `YAML`, `.md` for `Markdown`). If no extension, writes both
#'   formats
#' @param method Character. Output format: \code{"yaml"} (default),
#'   \code{"markdown"}, or \code{"both"}
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the path to written file
#'
#' @examples
#'
#' # Read from a package
#' mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#'
#' # Read a workflow
#' path <- system.file(
#'   "mcp", "workflows", "rave_pipeline_class_guide.yaml",
#'   package = "ravepipeline"
#' )
#'
#'
#' wf <- mcpflow_read(path)
#'
#' # Write as YAML to temporary file
#' mcpflow_write(wf, stdout(), method = "yaml")
#'
#' # Write as Markdown to temporary file
#' mcpflow_write(wf, stdout(), method = "markdown")
#'
#' @export
mcpflow_write <- function(workflow, path,
                          method = c("auto", "yaml", "markdown", "both"), ...) {

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
  if(!inherits(path, "connection")) {
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
    dir_create2(dirname(path))

  } else {
    if(method %in% c("auto", "both")) {
      method <- "yaml"
    }
    yaml_path <- path
    md_path <- path
  }

  written_files <- list()

  # Write YAML
  if (method %in% c("yaml", "both")) {
    # Remove S3 class before writing
    workflow_copy <- unclass(workflow)

    save_yaml(workflow_copy, yaml_path)
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

#' Validate RAVE MCP workflow
#'
#' @description Validates a workflow object for required fields and structure.
#'
#' @param workflow A RAVE workflow object or list
#' @param available_tools Character vector of available tool names.
#'   If NULL, skips tool reference validation
#'
#' @return A list with components:
#'   \item{valid}{Logical, whether workflow is valid}
#'   \item{errors}{Character vector of error messages (empty if valid)}
#'   \item{warnings}{Character vector of warning messages}
#'
#' @param strict Logical. If \code{TRUE}, perform full YAML schema validation
#'   via \code{mcpflow_validate_yaml}. Default is \code{FALSE} for lightweight
#'   validation of essential fields only.
#'
#' @examples
#'
#' # Read a workflow
#' path <- system.file(
#'   "mcp", "workflows", "rave_pipeline_class_guide.yaml",
#'   package = "ravepipeline"
#' )
#' wf <- mcpflow_read(path)
#'
#' # Validate (lightweight)
#' result <- mcpflow_validate(wf)
#' result$valid
#'
#' # Validate (strict schema validation)
#' result <- mcpflow_validate(wf, strict = TRUE)
#' result$valid
#'
#' @export
mcpflow_validate <- function(workflow, available_tools = NULL, strict = FALSE) {

  # If strict mode, delegate to full YAML schema validation
  if (isTRUE(strict)) {
    return(mcpflow_validate_yaml(workflow))
  }

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

  # Validate tool_guide references
  if (!is.null(workflow$tool_guide) && is.list(workflow$tool_guide)) {
    for (i in seq_along(workflow$tool_guide)) {
      tg <- workflow$tool_guide[[i]]
      if (!is.null(tg$tool) && is.character(tg$tool)) {
        if (!is.null(available_tools) && !tg$tool %in% available_tools) {
          warnings <- c(warnings, paste0(
            "tool_guide[", i, "] references unknown tool: '", tg$tool, "'"
          ))
        }
      }
    }
  }

  # Validate examples flow references
  if (!is.null(workflow$examples) && is.list(workflow$examples)) {
    for (i in seq_along(workflow$examples)) {
      ex <- workflow$examples[[i]]
      if (!is.null(ex$flow) && is.list(ex$flow)) {
        for (j in seq_along(ex$flow)) {
          fl <- ex$flow[[j]]
          if (!is.null(fl$tool) && is.character(fl$tool)) {
            if (!is.null(available_tools) && !fl$tool %in% available_tools) {
              warnings <- c(warnings, paste0(
                "examples[", i, "].flow[", j,
                "] references unknown tool: '", fl$tool, "'"
              ))
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

#' Find path to RAVE MCP workflow
#'
#' @description Extracts the file path for a workflow by name from
#'  \code{mcpflow_list} results.
#'
#' @param workflow_name Character. Workflow name to find
#' @param pkg Character. Package name. If NULL, uses path parameter
#' @param path Character. Path to workflows directory.
#'  If \code{pkg} is provided, uses `mcp/workflows` under the package folder
#'
#' @return Character string with the path to the workflow YAML file.
#'   Throws an error if workflow not found.
#'
#' @examples
#'
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


#' Load RAVE MCP workflows
#'
#' @description Loads workflow `YAML` files from a package's MCP workflows
#'  directory and returns them as `S3` objects with class
#'  \code{"ravepipeline_mcp_workflow"}.
#'
#' @param pkg Character string. Package name to load workflows from.
#'   Example values: \code{"ravepipeline"}
#' @param path Character string. Optional path to workflows directory.
#'   If provided, loads from this path instead of package `mcp/workflows`
#'   directory.
#'
#' @return A structured named list of workflow objects, or an empty list if no
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


#' Export RAVE MCP workflows
#'
#' @description Saves workflow objects to `YAML` and/or `Markdown` format.
#'    Optionally exports associated MCP tools to the same directory.
#'    Workflows are saved to `output_dir/workflows/` and tools to
#'    `output_dir/tools/`.
#'
#' @param workflows Either a \code{"ravepipeline_mcp_workflow"} object, a list
#'   of \code{"ravepipeline_mcp_workflow"} objects, or a character string path
#'   to a workflow `YAML` file
#' @param output_dir Character string, base directory to save output files.
#'   If \code{NULL} (default), uses the parent directory of the input file's
#'   folder (when workflows is a path) or current working directory
#'   (when workflows is an object). Workflows are saved to
#'   `output_dir/workflows/`, tools to `output_dir/tools/`
#' @param format Character string, output format. Options:
#'   \code{"markdown"} (default), \code{"yaml"}, \code{"both"}
#' @param save_tools Logical, whether to save referenced MCP tools to
#'   `output_dir/tools/`; default is \code{TRUE}
#' @param verbose Logical, whether to print progress messages;
#'   default is \code{TRUE}
#'
#' @return Invisibly returns a character vector of paths to written files
#'
#' @examples
#'
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



#' Extract tool names from workflows
#'
#' @description Recursively extracts all tool names referenced in workflow jobs
#'  and sections. Supports both single workflow objects and lists of workflows.
#'
#' @param workflows A single workflow object or a list of workflow objects
#' @return Character vector of unique tool names
#'
#' @examples
#'
#' # Extract tools from a single workflow
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#' tools <- mcpflow_tool_names(wf)
#'
#' # Extract tools from multiple workflows
#' workflows <- mcpflow_load_all("ravepipeline")
#' all_tools <- mcpflow_tool_names(workflows)
#'
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

    # Extract from tool_guide
    if (!is.null(workflow$tool_guide) && is.list(workflow$tool_guide)) {
      for (tg in workflow$tool_guide) {
        if (!is.null(tg$tool) && is.character(tg$tool)) {
          tools <- c(tools, tg$tool)
        }
      }
    }

    # Extract from examples (flow entries may have tool field)
    if (!is.null(workflow$examples) && is.list(workflow$examples)) {
      tools <- c(tools, find_tools_recursive(workflow$examples))
    }

    tools
  }

  # Helper to check if an object looks like a single workflow
  is_single_workflow <- function(x) {
    inherits(x, "ravepipeline_mcp_workflow") ||
      (is.list(x) && !is.null(x$name) && !is.null(x$jobs))
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
#' @description Loads and returns the MCP tools referenced by a workflow.
#'
#' @param workflow A workflow object
#' @param tools_dir Character. Base directory containing `tools/` subdirectory.
#'   Default is \code{NULL}, which uses the \code{'source_tools_dir'}
#'   attribute from the workflow or falls back to \code{"./tools"}.
#'
#' @return A named list of tool definitions
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
#' @description Converts a workflow `YAML` object to formatted `Markdown`
#'
#' @param workflow A MCP workflow object
#' @param sections Character vector of section names to include, or \code{NULL}
#'   to include all sections. Valid section names are: \code{"overview"},
#'   \code{"tool_guide"}, \code{"warnings"}, \code{"coding_guidelines"},
#'   \code{"code_generation_rules"}, \code{"best_practices"}, \code{"jobs"},
#'   \code{"examples"}. Header, description, metadata, MCP tools, and settings
#'   are always included regardless of this parameter.
#'
#' @return Character vector of markdown lines
#'
#' @keywords internal
convert_workflow_to_markdown <- function(workflow, sections = NULL) {
  # Define valid section names for filtering
  valid_sections <- c("overview", "tool_guide", "warnings", "coding_guidelines",
                      "code_generation_rules", "best_practices", "jobs", "examples")

  # Helper to check if a section should be included
  include_section <- function(section_name) {
    if (is.null(sections)) return(TRUE)
    section_name %in% sections
  }

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

  # Overview
  if (include_section("overview") && !is.null(workflow$overview) && nzchar(workflow$overview)) {
    lines <- c(lines, "## Overview")
    lines <- c(lines, "")
    lines <- c(lines, workflow$overview)
    lines <- c(lines, "")
  }

  # Tool Guide
  if (include_section("tool_guide") && !is.null(workflow$tool_guide) && is.list(workflow$tool_guide)) {
    lines <- c(lines, "## Tool Guide")
    lines <- c(lines, "")
    for (tg in workflow$tool_guide) {
      if (!is.null(tg$tool)) {
        lines <- c(lines, paste0("### `", tg$tool, "`"))
        lines <- c(lines, "")
        if (!is.null(tg$category)) {
          lines <- c(lines, paste0("**Category**: ", tg$category))
        }
        if (!is.null(tg$when)) {
          lines <- c(lines, paste0("**When to use**: ", tg$when))
        }
        if (!is.null(tg$notes)) {
          lines <- c(lines, paste0("**Notes**: ", tg$notes))
        }
        if (isTRUE(tg$dangerous)) {
          lines <- c(lines, "**WARNING**: This tool is dangerous")
        }
        if (isTRUE(tg$requires_approval)) {
          lines <- c(lines, "**Requires approval** before use")
        }
        if (!is.null(tg$preconditions) && length(tg$preconditions) > 0) {
          lines <- c(lines, "**Preconditions**:")
          for (pre in tg$preconditions) {
            lines <- c(lines, paste0("  - ", pre))
          }
        }
        lines <- c(lines, "")
      }
    }
  }

  # Jobs
  if (include_section("jobs") && !is.null(workflow$jobs) && is.list(workflow$jobs)) {
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
          if (!is.null(step$action)) {
            lines <- c(lines, paste0("   - Action: ", step$action))
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
            if (!is.null(step$loop$interval)) {
              loop_desc <- c(loop_desc, paste0("every ", step$loop$interval))
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
              param_val <- step$with[[param_name]]
              # Format arrays/lists as JSON-like
              if (is.list(param_val) || length(param_val) > 1) {
                param_str <- paste0("[", paste0('"', param_val, '"', collapse = ", "), "]")
              } else {
                param_str <- as.character(param_val)
              }
              lines <- c(lines, paste0("     - `", param_name, "`: ", param_str))
            }
          }
        }
        lines <- c(lines, "")
      }
    }
  }

  # Examples
  if (include_section("examples") && !is.null(workflow$examples) && is.list(workflow$examples)) {
    lines <- c(lines, "## Examples")
    lines <- c(lines, "")

    for (i in seq_along(workflow$examples)) {
      example <- workflow$examples[[i]]

      lines <- c(lines, paste0("### Example ", i))
      lines <- c(lines, "")

      # New schema: trigger/flow format
      if (!is.null(example$trigger)) {
        lines <- c(lines, paste0("**Trigger**: ", example$trigger))
        lines <- c(lines, "")
      }

      if (!is.null(example$flow) && is.list(example$flow)) {
        lines <- c(lines, "**Flow**:")
        lines <- c(lines, "")
        for (j in seq_along(example$flow)) {
          fl <- example$flow[[j]]
          step_desc <- ""
          if (!is.null(fl$tool)) {
            step_desc <- paste0("Tool: `", fl$tool, "`")
          } else if (!is.null(fl$action)) {
            step_desc <- paste0("Action: ", fl$action)
          }
          if (!is.null(fl$says) && nzchar(fl$says)) {
            step_desc <- paste0(step_desc, " - ", fl$says)
          }
          lines <- c(lines, paste0(j, ". ", step_desc))
        }
        lines <- c(lines, "")
      }
    }
  }

  # Warnings
  if (include_section("warnings") && !is.null(workflow$warnings) && length(workflow$warnings) > 0) {
    lines <- c(lines, "## Warnings")
    lines <- c(lines, "")
    for (warning_msg in workflow$warnings) {
      lines <- c(lines, paste0("- ", warning_msg))
    }
    lines <- c(lines, "")
  }

  # Coding Guidelines
  if (include_section("coding_guidelines") && !is.null(workflow$coding_guidelines) && length(workflow$coding_guidelines) > 0) {
    lines <- c(lines, "## Coding Guidelines")
    lines <- c(lines, "")
    lines <- c(lines, "**MANDATORY rules when generating R code:**")
    lines <- c(lines, "")
    for (guideline in workflow$coding_guidelines) {
      lines <- c(lines, paste0("- ", guideline))
    }
    lines <- c(lines, "")
  }

  # Code Generation Rules
  if (include_section("code_generation_rules") && !is.null(workflow$code_generation_rules) && is.list(workflow$code_generation_rules)) {
    cgr <- workflow$code_generation_rules
    lines <- c(lines, "## Code Generation Rules")
    lines <- c(lines, "")

    if (!is.null(cgr$description)) {
      lines <- c(lines, cgr$description)
      lines <- c(lines, "")
    }

    # Before writing code
    if (!is.null(cgr$before_writing_code) && length(cgr$before_writing_code) > 0) {
      lines <- c(lines, "### Before Writing Code")
      lines <- c(lines, "")
      for (rule in cgr$before_writing_code) {
        lines <- c(lines, paste0("- ", rule))
      }
      lines <- c(lines, "")
    }

    # Pipeline object methods
    if (!is.null(cgr$pipeline_object_methods) && is.list(cgr$pipeline_object_methods)) {
      lines <- c(lines, "### Pipeline Object Methods")
      lines <- c(lines, "")
      if (!is.null(cgr$pipeline_object_methods$correct)) {
        lines <- c(lines, "**Correct methods (USE THESE):**")
        lines <- c(lines, "")
        for (m in cgr$pipeline_object_methods$correct) {
          lines <- c(lines, paste0("- `", m, "`"))
        }
        lines <- c(lines, "")
      }
      if (!is.null(cgr$pipeline_object_methods$wrong)) {
        lines <- c(lines, "**Wrong methods (DO NOT USE):**")
        lines <- c(lines, "")
        for (m in cgr$pipeline_object_methods$wrong) {
          lines <- c(lines, paste0("- `", m, "`"))
        }
        lines <- c(lines, "")
      }
    }

    # Plotting
    if (!is.null(cgr$plotting) && is.list(cgr$plotting)) {
      lines <- c(lines, "### Plotting")
      lines <- c(lines, "")
      if (!is.null(cgr$plotting$do)) {
        lines <- c(lines, "**Do**:")
        lines <- c(lines, "")
        lines <- c(lines, cgr$plotting$do)
        lines <- c(lines, "")
      }
      if (!is.null(cgr$plotting$dont)) {
        lines <- c(lines, "**Don't**:")
        lines <- c(lines, "")
        lines <- c(lines, cgr$plotting$dont)
        lines <- c(lines, "")
      }
    }

    # Data manipulation
    if (!is.null(cgr$data_manipulation) && is.list(cgr$data_manipulation)) {
      lines <- c(lines, "### Data Manipulation")
      lines <- c(lines, "")
      if (!is.null(cgr$data_manipulation$do)) {
        lines <- c(lines, "**Do**:")
        lines <- c(lines, "")
        lines <- c(lines, cgr$data_manipulation$do)
        lines <- c(lines, "")
      }
      if (!is.null(cgr$data_manipulation$dont)) {
        lines <- c(lines, "**Don't**:")
        lines <- c(lines, "")
        lines <- c(lines, cgr$data_manipulation$dont)
        lines <- c(lines, "")
      }
    }

    # Result handling
    if (!is.null(cgr$result_handling) && length(cgr$result_handling) > 0) {
      lines <- c(lines, "### Result Handling")
      lines <- c(lines, "")
      for (rule in cgr$result_handling) {
        lines <- c(lines, paste0("- ", rule))
      }
      lines <- c(lines, "")
    }

    # Electrode selection
    if (!is.null(cgr$electrode_selection) && length(cgr$electrode_selection) > 0) {
      lines <- c(lines, "### Electrode Selection")
      lines <- c(lines, "")
      for (rule in cgr$electrode_selection) {
        lines <- c(lines, paste0("- ", rule))
      }
      lines <- c(lines, "")
    }
  }

  # Best Practices
  if (include_section("best_practices") && !is.null(workflow$best_practices) && length(workflow$best_practices) > 0) {
    lines <- c(lines, "## Best Practices")
    lines <- c(lines, "")
    for (bp in workflow$best_practices) {
      if (is.list(bp)) {
        if (!is.null(bp$title)) {
          lines <- c(lines, paste0("### ", bp$title))
          lines <- c(lines, "")
        }
        if (!is.null(bp$do)) {
          lines <- c(lines, "**Do**:")
          lines <- c(lines, "")
          lines <- c(lines, bp$do)
          lines <- c(lines, "")
        }
        if (!is.null(bp$dont)) {
          lines <- c(lines, "**Don't**:")
          lines <- c(lines, "")
          lines <- c(lines, bp$dont)
          lines <- c(lines, "")
        }
      }
    }
  }

  lines
}


#' @export
print.ravepipeline_mcp_workflow <- function(x, ...) {
  wf_name <- x$name %||% "unnamed"
  cat("RAVE MCP Workflow: <", wf_name, ">\n", sep = "")

  # Helper for truncating text
  truncate_text <- function(text, prefix_len = 0) {
    if (is.null(text) || !nzchar(text)) return(NULL)
    max_width <- getOption("width", 80L) - prefix_len - 4L
    # -4 for " ..." suffix
    text <- gsub("[\r\n]+", " ", text)  # collapse newlines
    if (nchar(text) > max_width) {
      text <- paste0(substr(text, 1, max_width), " ...")
    }
    text
  }

  if (!is.null(x$description)) {
    desc <- truncate_text(x$description, prefix_len = 16)
    # 16 = "  Description: "
    cat("  Description:", desc, "\n")
  }

  if (!is.null(x$version)) {
    cat("  Version:", x$version, "\n")
  }

  if (!is.null(x$category)) {
    cat("  Category:", x$category, "\n")
  }

  # Tools
  if (!is.null(x$mcp_tools) && length(x$mcp_tools) > 0) {
    tools_attached <- !is.null(attr(x, "mcp_tools"))
    suffix <- if (tools_attached) " (attached)" else ""
    cat("  MCP Tools: ", length(x$mcp_tools), suffix, "\n", sep = "")
  }

  # Jobs
  if (!is.null(x$jobs) && is.list(x$jobs)) {
    cat("  Jobs:", length(x$jobs), "\n")
  }

  # Examples
  if (!is.null(x$examples) && is.list(x$examples)) {
    cat("  Examples:", length(x$examples), "\n")
  }

  invisible(x)
}


#' @export
format.ravepipeline_mcp_workflow <- function(x, ...) {
  paste0("<RAVE MCP Workflow: ", x$name %||% "<unnamed>", ">")
}


#' Create validation tool for job execution
#'
#' @description Creates a callback function that validates tool calls against
#' workflow job definitions. The validation tool tracks which jobs have been completed
#' and warns (or rejects) when tools are called out of expected order or
#' when job dependencies are not met.
#'
#' @param workflow A MCP workflow object
#' @param strict Logical. If \code{TRUE}, reject out-of-order tool calls via
#'   \code{\link[ellmer]{tool_reject}}. If \code{FALSE} (default), only warn via
#'   \code{\link[cli]{cli_warn}}.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages
#'   when jobs and steps are detected.
#'
#' @return A list with two callback functions:
#'   \item{\code{on_tool_request}}{Callback for \code{chat$on_tool_request()}}
#'   \item{\code{on_tool_result}}{Callback for \code{chat$on_tool_result()}}
#'   \item{\code{state}}{An environment containing execution state (for inspection)}
#'
#' @details
#' The validation tool maintains state about which jobs have been
#' started/completed based on tool calls observed. It checks:
#' \itemize{
#'   \item Whether the tool being called matches expected job steps
#'   \item Whether job dependencies (\code{needs}) are satisfied
#'   \item Step execution order within jobs
#' }
#'
#' @examples
#'
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#' validator <- mcpflow_job_validator(wf, strict = FALSE)
#'
#' \dontrun{
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


#' Create workflow guidance tool
#'
#' @description Creates an \code{'ellmer'} tool that provides on-demand access
#' to workflow documentation sections. This is used internally by
#' \code{\link{mcpflow_instantiate}} when a workflow has \code{prompt_sections}
#' defined, allowing the AI to fetch detailed documentation only when needed
#' to reduce initial system prompt size.
#'
#' @param workflow A \code{'ravepipeline_mcp_workflow'} object.
#' @param sections Character vector of available section names.
#'
#' @return An \code{'ellmer'} tool object.
#'
#' @keywords internal
mcpflow_guidance_tool <- function(workflow, sections) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required to create guidance tool.")
  }

  # Build description
  sections_list <- paste(sprintf("'%s'", sections), collapse = ", ")
  description <- sprintf(
    "Retrieves detailed workflow guidance for a specific section. Available sections: %s. Use this tool when you need more detailed information about a particular aspect of the workflow that is not in the core system prompt.",
    sections_list
  )

  # Create the tool function that captures workflow in closure
  guidance_fn <- function(section) {
    if (!section %in% sections) {
      return(sprintf("Invalid section '%s'. Available sections: %s",
                     section, paste(sections, collapse = ", ")))
    }
    # Generate markdown for just this section
    md <- convert_workflow_to_markdown(workflow, sections = section)
    if (length(md) == 0 || all(!nzchar(md))) {
      return(sprintf("Section '%s' is empty or not defined in this workflow.", section))
    }
    paste(md, collapse = "\n")
  }

  # Build arguments list for the tool
  arguments <- list(
    section = ellmer::type_enum(
      values = sections,
      description = "The workflow documentation section to retrieve"
    )
  )

  ellmer::tool(
    fun = guidance_fn,
    name = "get_workflow_guidance",
    description = description,
    arguments = arguments
  )
}


#' Instantiate workflow as a chat
#'
#' @description Creates a chat object configured with the workflow's
#' system prompt (derived from workflow content) and all referenced tools
#' registered. Optionally attaches validation callbacks to track job execution.
#'
#' @param workflow A \code{'ravepipeline_mcp_workflow'} object, or a
#'   path/identifier that can be read via \code{\link{mcpflow_read}}.
#' @param chat An existing \code{'ellmer'} chat object to configure. If \code{NULL}
#'   (default), a new chat is created using \code{chat_provider}.
#' @param chat_provider Character. The chat model to use when creating a
#'   new chat. One of \code{"ollama"} (default), \code{"openai"},
#'   \code{"claude"}, \code{"gemini"}, \code{"cortex"}, \code{"azure_openai"},
#'   \code{"bedrock"}, \code{"databricks"}, \code{"github"}, \code{"groq"},
#'   \code{"perplexity"}, \code{"snowflake"}, \code{"vllm"}. Only used when
#'   \code{chat} is \code{NULL}.
#' @param chat_args A named list of additional arguments passed to the
#'   \code{'ellmer'} constructor (e.g., \code{model}, \code{api_key},
#'   \code{base_url}). Only used when \code{chat} is \code{NULL}.
#' @param on_tool_request Optional callback function for tool request events.
#'   Passed to \code{chat$on_tool_request()}. Receives a "content tool request"
#'   object. Can call \code{\link[ellmer]{tool_reject}} to prevent execution.
#' @param on_tool_result Optional callback function for tool result events.
#'   Passed to \code{chat$on_tool_result()}. Receives a "content tool result"
#'   object.
#' @param use_job_validator Logical. If \code{TRUE}, automatically validate
#'   jobs by \code{\link{mcpflow_job_validator}}. Default is
#'   \code{FALSE}. Ignored if \code{on_tool_request} or \code{on_tool_result}
#'   are provided.
#' @param validator_strict Logical. If \code{TRUE} and \code{use_job_validator}
#'   is \code{TRUE}, the validation tool will reject out-of-order tool calls.
#'   Default is \code{FALSE} (advisory warnings only).
#' @param validator_verbose Logical. If \code{TRUE} (default) and
#'   \code{use_job_validator} is \code{TRUE}, print progress messages.
#' @param state_env Environment or \code{NULL} (default). Environment in which
#'   the MCP tools share and store data; see \code{\link{mcptool_state_factory}}
#' @param ... Additional arguments passed to \code{\link{mcptool_instantiate}}
#'   for each tool.
#'
#' @return An \code{'ellmer'} chat object with:
#'   \itemize{
#'     \item System prompt set from workflow content (via \code{\link{convert_workflow_to_markdown}})
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
#' # Load workflow and create chat with Ollama
#' wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#'
#' # This example requires connecting to external service providers
#' \dontrun{
#' # Ollama (default) might require explicit model
#' chat <- mcpflow_instantiate(wf, chat_args = list(model = "qwen3:8b"))
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
#' chat <- mcpflow_instantiate(wf, use_job_validator = TRUE,
#'                             validator_strict = TRUE)
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
    state_env = NULL,
    ...
) {
  # Check ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required to instantiate workflow chat.")
  }
  ellmer <- asNamespace("ellmer")

  # Ensure workflow is loaded with tools
  workflow <- mcpflow_read(workflow, tools = TRUE)

  state_env <- mcptool_state_factory(.state = state_env)

  # Create or validate chat object
  if (is.null(chat)) {
    chat_provider <- match.arg(chat_provider)
    chat_constructor <- ellmer[[sprintf("chat_%s", chat_provider)]]

    if (is.null(chat_constructor) || !is.function(chat_constructor)) {
      stop("ellmer does not have a 'chat_", chat_provider, "' function.")
    }

    chat <- do.call(chat_constructor, chat_args)
  }

  attr(chat, "rave_state_env") <- state_env

  # Define all possible sections

  all_sections <- c("overview", "tool_guide", "warnings", "coding_guidelines",
                    "code_generation_rules", "best_practices", "jobs", "examples")

  # Check if workflow has prompt_sections for lazy loading
  prompt_sections <- workflow$prompt_sections
  on_demand_sections <- NULL

  if (!is.null(prompt_sections) && !is.null(prompt_sections$core)) {
    # Use only core sections in system prompt
    core_sections <- prompt_sections$core
    on_demand_sections <- setdiff(all_sections, core_sections)
    # Filter to only sections that actually exist in the workflow
    on_demand_sections <- on_demand_sections[vapply(on_demand_sections, function(s) {
      !is.null(workflow[[s]]) && length(workflow[[s]]) > 0
    }, logical(1))]

    system_prompt <- paste(convert_workflow_to_markdown(workflow, sections = core_sections), collapse = "\n")

    # Append guidance about available on-demand sections
    if (length(on_demand_sections) > 0) {
      system_prompt <- paste0(
        system_prompt,
        "\n\n---\n\n",
        "**Additional Documentation Available**\n\n",
        "The following sections are available on-demand via the `get_workflow_guidance` tool: ",
        paste(sprintf("`%s`", on_demand_sections), collapse = ", "),
        ". Use this tool when you need detailed information about these topics."
      )
    }
  } else {
    # No prompt_sections defined - include everything
    system_prompt <- paste(convert_workflow_to_markdown(workflow), collapse = "\n")
  }

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
        ellmer_tool <- mcptool_instantiate(tool, ..., state_env = state_env)
        chat$register_tool(ellmer_tool)
      }
    }
  }

  # Register guidance tool for on-demand sections
  if (!is.null(on_demand_sections) && length(on_demand_sections) > 0) {
    guidance_tool <- mcpflow_guidance_tool(workflow, on_demand_sections)
    chat$register_tool(guidance_tool)
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
