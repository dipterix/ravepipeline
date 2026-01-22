# ---- MCP Tool Loading Tests ----

test_that("mcptool_load_all loads all package MCP tools", {
  tools <- mcptool_load_all("ravepipeline")

  expect_true(length(tools) >= 7)
  expect_true(all(vapply(tools, inherits, logical(1), "ravepipeline_mcp_tool")))

  # Check tool names use hyphen format
  tool_names <- vapply(tools, `[[`, character(1), "name")
  expect_true(all(grepl("^ravepipeline-", tool_names)))
})

test_that("mcptool_load_all loads from directory path", {
  tools_dir <- system.file("mcp", "tools", package = "ravepipeline")
  tools <- mcptool_load_all(tools_dir)

  expect_true(length(tools) >= 7)
  expect_true(all(vapply(tools, inherits, logical(1), "ravepipeline_mcp_tool")))
})

test_that("mcptool_read loads individual tool YAML", {
  tools_dir <- system.file("mcp", "tools", package = "ravepipeline")
  yaml_files <- list.files(tools_dir, pattern = "\\.yaml$", full.names = TRUE)

  skip_if(length(yaml_files) == 0, "No tool YAML files found")


  tool <- mcptool_read(yaml_files[1])

  expect_s3_class(tool, "ravepipeline_mcp_tool")
  expect_true(!is.null(tool$name))
  expect_true(!is.null(tool$description))
})

test_that("mcptool_path finds tool by name", {
  # Test finding a known tool
  path <- mcptool_path("ravepipeline-mcp_tool_pipeline_list")

  expect_true(!is.null(path))
  expect_true(file.exists(path))
  expect_match(path, "\\.yaml$")
})

test_that("mcptool_seek_function resolves tool to function", {
  tools <- mcptool_load_all("ravepipeline")
  skip_if(length(tools) == 0, "No tools loaded")

  # Get the first tool and resolve its function
  tool <- tools[[1]]
  fn <- mcptool_seek_function(tool)

  expect_true(is.function(fn))
})


# ---- ellmer Tool Instantiation Tests ----

test_that("mcptool_instantiate creates valid ellmer ToolDef objects", {
  skip_if_not_installed("ellmer")

  tools <- mcptool_load_all("ravepipeline")
  expect_true(length(tools) > 0)

  for (tool in tools) {
    ellmer_tool <- tryCatch(
      mcptool_instantiate(tool),
      error = function(e) {
        fail(sprintf("Tool '%s' failed to instantiate: %s", tool$name, conditionMessage(e)))
      }
    )

    # Check it's a ToolDef (S7 class reports as "ellmer::ToolDef")
    expect_true(any(grepl("ToolDef", class(ellmer_tool))))

    # Check it's callable (ToolDef inherits from function)
    expect_true(is.function(ellmer_tool))

    # Check name format compliance (ellmer only allows letters, digits, _, -)
    expect_match(ellmer_tool@name, "^[a-zA-Z0-9_-]+$")

    # Check description exists
    expect_true(nchar(ellmer_tool@description) > 0)
  }
})

test_that("mcptool_instantiate handles enum parameters correctly", {
  skip_if_not_installed("ellmer")

  # Find a tool with enum parameter (mcp_get_current_rave_pipeline_progress has one)
  tools <- mcptool_load_all("ravepipeline")
  progress_tool <- NULL
  for (tool in tools) {
    if (grepl("progress", tool$name)) {
      progress_tool <- tool
      break
    }
  }

  skip_if(is.null(progress_tool), "No progress tool found")

  # Should instantiate without error
  ellmer_tool <- mcptool_instantiate(progress_tool)
  expect_true(any(grepl("ToolDef", class(ellmer_tool))))

  # Check arguments include the enum parameter
  args <- ellmer_tool@arguments
  expect_true(!is.null(args@properties$detail_level))
})

test_that("mcptool_instantiate tool names match original tool names", {
  skip_if_not_installed("ellmer")

  tools <- mcptool_load_all("ravepipeline")

  for (tool in tools) {
    ellmer_tool <- tryCatch(
      mcptool_instantiate(tool),
      error = function(e) {
        fail(sprintf("Tool '%s' failed to instantiate: %s", tool$name, conditionMessage(e)))
      }
    )
    # The ellmer tool name should match the original (possibly with invalid chars replaced)
    expected_name <- gsub("[^a-zA-Z0-9_-]+", "-", tool$name)
    expect_equal(ellmer_tool@name, expected_name)
  }
})

# ---- Comprehensive Tool Instantiation Tests ----

test_that("all MCP tools can be instantiated without error", {
  skip_if_not_installed("ellmer")

  # Load all tools from the ravepipeline package
  all_tools <- mcptool_load_all("ravepipeline")

  expect_true(length(all_tools) > 0, "Expected at least one MCP tool to be available")

  # Track any failures for better error reporting
  failed_tools <- character(0)
  instantiation_errors <- list()

  # Attempt to instantiate each tool
  for (tool in all_tools) {
    tool_name <- tool$name

    result <- tryCatch({
      ellmer_tool <- mcptool_instantiate(tool)

      # Verify the instantiated tool is valid
      expect_true(any(grepl("ToolDef", class(ellmer_tool))),
                  info = sprintf("Tool '%s' should be a ToolDef", tool_name))
      expect_true(is.function(ellmer_tool),
                  info = sprintf("Tool '%s' should be callable", tool_name))
      expect_true(nchar(ellmer_tool@name) > 0,
                  info = sprintf("Tool '%s' should have a name", tool_name))
      expect_true(nchar(ellmer_tool@description) > 0,
                  info = sprintf("Tool '%s' should have a description", tool_name))

      TRUE
    }, error = function(e) {
      failed_tools <<- c(failed_tools, tool_name)
      instantiation_errors[[tool_name]] <<- conditionMessage(e)
      FALSE
    })

    expect_true(result,
                info = sprintf("Failed to instantiate tool '%s': %s",
                              tool_name,
                              instantiation_errors[[tool_name]] %||% "unknown error"))
  }

  # Final summary check
  if (length(failed_tools) > 0) {
    error_summary <- paste(
      sprintf("\nFailed to instantiate %d tool(s):", length(failed_tools)),
      paste(sprintf("  - %s: %s", failed_tools, unlist(instantiation_errors)), collapse = "\n"),
      sep = "\n"
    )
    fail(error_summary)
  }

  # Report success
  message(sprintf("Successfully instantiated all %d MCP tools", length(all_tools)))
})

test_that("all MCP tools by group can be instantiated", {
  skip_if_not_installed("ellmer")

  # Test instantiation for each tool group
  groups <- c("pipeline", "docs", "config", "workflow")

  for (group in groups) {
    group_tools <- mcptool_load_all("ravepipeline", groups = group)

    # Skip if no tools in this group
    if (length(group_tools) == 0) {
      skip(sprintf("No tools found in group '%s'", group))
    }

    for (tool in group_tools) {
      tryCatch(
        mcptool_instantiate(tool),
        error = function(e) {
          fail(sprintf("Tool '%s' from group '%s' failed to instantiate: %s",
                      tool$name, group, conditionMessage(e)))
        }
      )
    }

    message(sprintf("Group '%s': Successfully instantiated %d tool(s)", group, length(group_tools)))
  }
})
