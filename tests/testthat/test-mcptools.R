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
  path <- mcptool_path("ravepipeline-mcp_list_rave_pipelines")

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
    ellmer_tool <- mcptool_instantiate(tool)

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
    ellmer_tool <- mcptool_instantiate(tool)
    # The ellmer tool name should match the original (possibly with invalid chars replaced)
    expected_name <- gsub("[^a-zA-Z0-9_-]+", "-", tool$name)
    expect_equal(ellmer_tool@name, expected_name)
  }
})
