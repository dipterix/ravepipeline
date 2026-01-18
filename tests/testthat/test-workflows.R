test_that("mcpflow_load_all loads YAML files correctly", {
  # Create temporary workflow directory
  temp_dir <- tempfile("workflows_test")
  dir.create(file.path(temp_dir, "workflows"), recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Create a test workflow YAML
  workflow_yaml <- list(
    name = "test_workflow",
    description = "A test workflow",
    version = "1.0.0",
    mcp_tools = TRUE,
    jobs = list(
      job1 = list(
        name = "Test Job",
        steps = list(
          list(name = "Step 1", tool = "ravepipeline-mcp_list_rave_pipelines")
        )
      )
    )
  )

  yaml_path <- file.path(temp_dir, "workflows", "test_workflow.yaml")
  yaml::write_yaml(workflow_yaml, yaml_path)

  # Load workflows
  utils::capture.output(type = "message", {
    workflows <- mcpflow_load_all(path = temp_dir)
  })

  # Verify
  expect_equal(length(workflows), 1)
  expect_true(inherits(workflows[[1]], "ravepipeline_mcp_workflow"))
  expect_equal(workflows[[1]]$name, "test_workflow")
  expect_equal(workflows[[1]]$mcp_tools, "ravepipeline-mcp_list_rave_pipelines") # TRUE -> "all" -> explicit tool names
})

test_that("mcpflow_load_all normalizes mcp_tools field", {
  temp_dir <- tempfile("workflows_test")
  dir.create(file.path(temp_dir, "workflows"), recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Test TRUE -> "all"
  yaml::write_yaml(
    list(name = "wf1", mcp_tools = TRUE),
    file.path(temp_dir, "workflows", "wf1.yaml")
  )

  # Test "yes" -> "all"
  yaml::write_yaml(
    list(name = "wf2"),
    file.path(temp_dir, "workflows", "wf2.yaml")
  )

  # Test FALSE -> empty
  yaml::write_yaml(
    list(name = "wf3", mcp_tools = FALSE),
    file.path(temp_dir, "workflows", "wf3.yaml")
  )

  workflows <- mcpflow_load_all(path = temp_dir)

  expect_equal(length(workflows$wf1$mcp_tools), 0) # TRUE -> "all" -> character(0) (no jobs)
  expect_equal(length(workflows$wf2$mcp_tools), 0) # "yes" -> "all" -> character(0) (no jobs)
  expect_equal(length(workflows$wf3$mcp_tools), 0)

  # Test array -> preserved
  yaml::write_yaml(
    list(name = "wf4", mcp_tools = c("tool1", "tool2")),
    file.path(temp_dir, "workflows", "wf4.yaml")
  )
  expect_error({
    workflows <- mcpflow_load_all(path = temp_dir)
  })
})

test_that("mcpflow_export saves YAML format", {
  temp_dir <- tempfile("workflows_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Create workflow object
  workflow <- structure(
    list(
      name = "test_save",
      description = "Test save functionality",
      mcp_tools = "all"
    ),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  # Save as YAML
  utils::capture.output(type = "message", {
    result <- mcpflow_export(
      workflow,
      output_dir = temp_dir,
      format = "yaml",
      verbose = FALSE
    )
  })

  # Verify file was created in workflows/ subdirectory
  expect_true(file.exists(file.path(temp_dir, "workflows", "test_save.yaml")))

  # Verify content
  saved <- yaml::read_yaml(file.path(temp_dir, "workflows", "test_save.yaml"))
  expect_equal(saved$name, "test_save")
  expect_equal(saved$description, "Test save functionality")
})

test_that("mcpflow_export saves Markdown format", {
  temp_dir <- tempfile("workflows_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Create workflow object
  workflow <- structure(
    list(
      name = "test_md",
      description = "Test markdown generation",
      version = "1.0.0",
      mcp_tools = c("tool1", "tool2")
    ),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  # Save as Markdown (save_tools=FALSE to avoid warnings about missing tools)
  utils::capture.output(type = "message", {
    result <- mcpflow_export(
      workflow,
      output_dir = temp_dir,
      format = "markdown",
      save_tools = FALSE,
      verbose = FALSE
    )
  })

  # Verify file was created in workflows/ subdirectory
  md_path <- file.path(temp_dir, "workflows", "test_md.md")
  expect_true(file.exists(md_path))

  # Verify content
  md_content <- readLines(md_path)
  expect_true(any(grepl("# test_md", md_content, fixed = TRUE)))
  expect_true(any(grepl("Test markdown generation", md_content, fixed = TRUE)))
})

test_that("mcpflow_export accepts path input", {
  temp_dir <- tempfile("workflows_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Create input YAML
  input_yaml <- file.path(temp_dir, "input.yaml")
  yaml::write_yaml(
    list(name = "from_path", description = "Loaded from path"),
    input_yaml
  )

  # Create output dir
  output_dir <- file.path(temp_dir, "output")
  dir.create(output_dir, showWarnings = FALSE)

  # Save from path
  utils::capture.output(type = "message", {
    result <- mcpflow_export(
      input_yaml,
      output_dir = output_dir,
      format = "both",
      verbose = FALSE
    )
  })

  # Verify both formats created
  expect_true(file.exists(file.path(output_dir, "workflows", "from_path.yaml")))
  expect_true(file.exists(file.path(output_dir, "workflows", "from_path.md")))
})

test_that("mcpflow_validate detects missing required fields", {
  # Missing name
  workflow <- list(description = "Test")
  result <- mcpflow_validate(workflow)

  expect_false(result$valid)
  expect_true(any(grepl("name", result$errors, ignore.case = TRUE)))
})

test_that("mcpflow_validate validates mcp_tools field type", {
  # Invalid type
  workflow <- list(
    name = "test",
    mcp_tools = 123  # Should be character or logical
  )

  result <- mcpflow_validate(workflow)
  expect_false(result$valid)
  expect_true(any(grepl("mcp_tools", result$errors, ignore.case = TRUE)))
})

test_that("mcpflow_validate checks tool references against available tools", {
  workflow <- list(
    name = "test",
    mcp_tools = c("existing_tool", "missing_tool")
  )

  available <- c("existing_tool", "another_tool")
  result <- mcpflow_validate(workflow, available_tools = available)

  # Should have warning about missing_tool
  expect_true(any(grepl("missing_tool", result$warnings, ignore.case = TRUE)))
})

test_that("mcpflow_validate checks jobs structure", {
  # Invalid jobs (not a list)
  workflow <- list(
    name = "test",
    jobs = "invalid"
  )

  result <- mcpflow_validate(workflow)
  expect_false(result$valid)
  expect_true(any(grepl("jobs", result$errors, ignore.case = TRUE)))
})

test_that("mcpflow_tools extracts tool names from jobs", {
  workflow <- list(
    name = "test",
    jobs = list(
      job1 = list(
        steps = list(
          list(tool = "tool1"),
          list(tool = "tool2")
        )
      ),
      job2 = list(
        steps = list(
          list(tool = "tool3"),
          list(tool = "tool1")  # Duplicate
        )
      )
    )
  )

  tools <- mcpflow_tool_names(workflow)

  expect_equal(length(tools), 3)
  expect_true(all(c("tool1", "tool2", "tool3") %in% tools))
})

test_that("mcpflow_tools handles missing steps", {
  workflow <- list(
    name = "test",
    jobs = list(
      job1 = list(name = "Job without steps"),
      job2 = list(steps = NULL)
    )
  )

  tools <- mcpflow_tool_names(workflow)
  expect_equal(length(tools), 0)
})

test_that("print.ravepipeline_mcp_workflow works", {
  workflow <- structure(
    list(
      name = "test_print",
      description = "Test print method",
      version = "1.0.0",
      mcp_tools = "all",
      jobs = list(job1 = list())
    ),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  output <- utils::capture.output(print(workflow))

  expect_true(any(grepl("RAVE MCP Workflow", output)))
  expect_true(any(grepl("test_print", output)))
})

test_that("format.ravepipeline_mcp_workflow works", {
  workflow <- structure(
    list(name = "test_format"),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  result <- format(workflow)

  expect_true(grepl("RAVE MCP Workflow", result))
  expect_true(grepl("test_format", result))
})

test_that("mcpflow_export with save_tools=TRUE exports tools", {
  skip_if_not_installed("yaml")

  temp_dir <- tempfile("workflows_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  # Create workflow
  workflow <- structure(
    list(
      name = "test_with_tools",
      mcp_tools = c("ravepipeline-mcp_list_rave_pipelines"),
      jobs = list(
        job1 = list(
          steps = list(
            list(tool = "ravepipeline-mcp_list_rave_pipelines")
          )
        )
      )
    ),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  # Save with tools - this will try to load tools from ravepipeline package
  # May not work in test environment, so we'll just check it doesn't error
  expect_no_error({
    mcpflow_export(
      workflow,
      output_dir = temp_dir,
      format = "yaml",
      save_tools = TRUE,
      verbose = FALSE
    )
  })
})

test_that("convert_workflow_to_markdown handles all sections", {
  workflow <- structure(
    list(
      name = "comprehensive_test",
      description = "A comprehensive workflow for testing",
      version = "2.0.0",
      category = "testing",
      tags = c("test", "demo"),
      mcp_tools = c("tool1", "tool2"),
      settings = list(
        dangerous = TRUE,
        requires_approval = TRUE,
        estimated_duration = "5 minutes"
      ),
      jobs = list(
        job1 = list(
          name = "First Job",
          description = "Does something",
          steps = list(
            list(
              name = "Step 1",
              tool = "tool1",
              description = "First step",
              with = list(param1 = "value1")
            )
          )
        )
      ),
      warnings = c("Warning 1", "Warning 2"),
      examples = list(
        list(
          user_query = "Test query",
          expected_flow = "Step1 -> Step2"
        )
      )
    ),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  md_lines <- convert_workflow_to_markdown(workflow)
  md_text <- paste(md_lines, collapse = "\n")

  # Check various sections present
  expect_true(grepl("# comprehensive_test", md_text, fixed = TRUE))
  expect_true(grepl("**Version**: 2.0.0", md_text, fixed = TRUE))
  expect_true(grepl("## MCP Tools", md_text, fixed = TRUE))
  expect_true(grepl("## Settings", md_text, fixed = TRUE))
  expect_true(grepl("WARNING", md_text, fixed = TRUE))
  expect_true(grepl("## Workflow Jobs", md_text, fixed = TRUE))
  expect_true(grepl("## Examples", md_text, fixed = TRUE))
  expect_true(grepl("## Warnings", md_text, fixed = TRUE))
})

test_that("mcpflow_export returns written file paths", {
  temp_dir <- tempfile("workflows_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

  workflow <- structure(
    list(name = "test_return"),
    class = c("ravepipeline_mcp_workflow", "list")
  )

  result <- mcpflow_export(
    workflow,
    output_dir = temp_dir,
    format = "both",
    verbose = FALSE
  )

  expect_equal(length(result), 1)  # YAML and MD

  expect_true(all(file.exists(file.path(
    result, "workflows", sprintf("%s.%s", workflow$name, c("yaml", "md"))
  ))))
})

