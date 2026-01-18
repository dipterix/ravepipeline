# Test loading the actual workflow YAML

devtools::load_all()

# Load the workflow
workflows <- mcpflow_load_all("ravepipeline")

cat("=== Loaded Workflows ===\n")
print(names(workflows))

if (length(workflows) > 0) {
  cat("\n=== First Workflow ===\n")
  print(workflows[[1]])

  cat("\n=== Workflow Details ===\n")
  wf <- workflows[[1]]
  cat("Name:", wf$name, "\n")
  cat("Description:", substr(wf$description, 1, 100), "...\n")
  cat("MCP Tools:", wf$mcp_tools, "\n")
  cat("Number of sections:", length(wf$sections), "\n")

  # Test saving to markdown
  cat("\n=== Saving to Markdown ===\n")
  result <- mcpflow_export(
    wf,
    output_dir = "adhoc",
    format = "markdown",
    verbose = TRUE
  )

  # Show first 30 lines
  cat("\n=== First 30 lines of generated markdown ===\n")
  md <- readLines(file.path(result, "workflows/rave_pipeline_class_guide.md"))
  cat(paste(head(md, 30), collapse = "\n"))
}
