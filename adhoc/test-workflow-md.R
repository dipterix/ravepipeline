# Test workflow markdown conversion

devtools::load_all()

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

md_lines <- ravepipeline:::convert_workflow_to_markdown(workflow)
md_text <- paste(md_lines, collapse = "\n")

cat("=== Generated Markdown ===\n")
cat(md_text)
cat("\n\n=== Checking for Version ===\n")
cat("Has 'Version: 2.0.0':", grepl("Version: 2.0.0", md_text, fixed = TRUE), "\n")
cat("Has 'Version':", grepl("Version", md_text, fixed = TRUE), "\n")
cat("\n=== First 20 lines ===\n")
cat(paste(head(md_lines, 20), collapse = "\n"))
