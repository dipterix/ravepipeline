# Test mcpflow_export with new structure
# Load package functions
devtools::load_all(".")

# Create temporary test directory
test_dir <- tempfile("workflow_test_")
dir.create(test_dir, recursive = TRUE)
cat("Test directory:", test_dir, "\n\n")

cat("=== Test 1: Load existing workflow ===\n")
workflows <- mcpflow_load_all(path = "./inst/mcp")
cat("Loaded", length(workflows), "workflow(s)\n")
if (length(workflows) > 0) {
  wf <- workflows[[1]]
  cat("Workflow name:", wf$name, "\n")
  cat("mcp_tools field:",
      if (identical(wf$mcp_tools, "all")) "all" else paste(head(wf$mcp_tools, 3), collapse = ", "),
      "\n")
}

cat("\n=== Test 2: Save workflow to new location (save_tools = FALSE) ===\n")
if (length(workflows) > 0) {
  files <- mcpflow_export(
    workflows[[1]],
    output_dir = file.path(test_dir, "test1"),
    format = "both",
    save_tools = FALSE,
    verbose = TRUE
  )

  cat("\nWritten files:\n")
  print(files)

  cat("\nDirectory structure:\n")
  system(paste("find", file.path(test_dir, "test1"), "-type f"))
}

cat("\n=== Test 3: Save workflow with tools (save_tools = TRUE) ===\n")
if (length(workflows) > 0) {
  files <- mcpflow_export(
    workflows[[1]],
    output_dir = file.path(test_dir, "test2"),
    format = "yaml",
    save_tools = TRUE,
    verbose = TRUE
  )

  cat("\nWritten files:\n")
  print(files)

  cat("\nDirectory structure:\n")
  system(paste("find", file.path(test_dir, "test2"), "-type f"))

  # Check if 'all' was replaced
  saved_yaml <- file.path(test_dir, "test2", "workflows", paste0(workflows[[1]]$name, ".yaml"))
  if (file.exists(saved_yaml)) {
    saved_wf <- yaml::read_yaml(saved_yaml)
    cat("\nSaved workflow mcp_tools field type:", class(saved_wf$mcp_tools), "\n")
    cat("Number of tools:", length(saved_wf$mcp_tools), "\n")
    cat("First few tools:", paste(head(saved_wf$mcp_tools, 3), collapse = ", "), "\n")
  }
}

cat("\n=== Test 4: Save workflow from path ===\n")
if (length(workflows) > 0) {
  # First save a workflow
  workflow_path <- file.path(test_dir, "test3", "workflows", paste0(workflows[[1]]$name, ".yaml"))
  mcpflow_export(
    workflows[[1]],
    output_dir = file.path(test_dir, "test3"),
    format = "yaml",
    save_tools = FALSE,
    verbose = FALSE
  )

}

cat("\n=== Test 5: Save all workflows ===\n")
if (length(workflows) > 0) {
  files <- mcpflow_export(
    workflows,
    output_dir = file.path(test_dir, "test4"),
    format = "markdown",
    save_tools = TRUE,
    verbose = TRUE
  )

  cat("\nTotal files written:", length(files), "\n")
  cat("\nDirectory structure:\n")
  system(paste("find", file.path(test_dir, "test4"), "-type f"))
}

cat("\n=== Test 6: Verify tool filename conversion ===\n")
if (length(workflows) > 0) {
  mcpflow_export(
    workflows[[1]],
    output_dir = file.path(test_dir, "test5"),
    format = "yaml",
    save_tools = TRUE,
    verbose = FALSE
  )

  tools_dir <- file.path(test_dir, "test5", "tools")
  if (dir.exists(tools_dir)) {
    tool_files <- list.files(tools_dir, pattern = "\\.yaml$")
    cat("Tool files created:\n")
    for (f in head(tool_files, 5)) {
      cat(" ", f, "\n")
    }
    cat("...(", length(tool_files), "total)\n")
  }
}

cat("\n=== Test 7: Test with workflow object (not from path) ===\n")
if (length(workflows) > 0) {
  files <- mcpflow_export(
    workflows[[1]],
    output_dir = file.path(test_dir, "test6"),
    format = "both",
    save_tools = TRUE,
    verbose = TRUE
  )

}

cat("\n=== Cleanup ===\n")
cat("Test files are in:", test_dir, "\n")
cat("To clean up, run: unlink('", test_dir, "', recursive = TRUE)\n")

cat("\n=== All tests completed ===\n")
