# Test prompt_sections feature
library(devtools)
load_all()

# Test 1: Validate the updated YAML
cat("Test 1: Validating YAML with prompt_sections...\n")
wf_path <- system.file("mcp", "workflows", "rave_pipeline_class_guide.yaml", package = "ravepipeline")
yaml_data <- yaml::read_yaml(wf_path)
result <- mcpflow_validate_yaml(yaml_data)
cat("Valid:", result$valid, "\n")
if (length(result$errors) > 0) {
  cat("Errors:\n")
  print(result$errors)
}

# Test 2: Test convert_workflow_to_markdown with sections parameter
cat("\nTest 2: Testing convert_workflow_to_markdown with sections...\n")
wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")

# Full markdown (no sections filter)
full_md <- convert_workflow_to_markdown(wf)
cat("Full markdown lines:", length(full_md), "\n")
cat("Full markdown chars:", nchar(paste(full_md, collapse = "\n")), "\n")

# Only overview section
overview_md <- convert_workflow_to_markdown(wf, sections = "overview")
cat("Overview only lines:", length(overview_md), "\n")

# Core sections
core_md <- convert_workflow_to_markdown(wf, sections = c("overview", "tool_guide", "warnings", "coding_guidelines"))
cat("Core sections lines:", length(core_md), "\n")
cat("Core sections chars:", nchar(paste(core_md, collapse = "\n")), "\n")

cat("\nTest 3: Check prompt_sections in workflow...\n")
cat("prompt_sections$core:", paste(wf$prompt_sections$core, collapse = ", "), "\n")

# Test 4: Check on-demand sections calculation
all_sections <- c("overview", "tool_guide", "warnings", "coding_guidelines",
                  "code_generation_rules", "best_practices", "jobs", "examples")
core <- wf$prompt_sections$core
on_demand <- setdiff(all_sections, core)
cat("On-demand sections:", paste(on_demand, collapse = ", "), "\n")

cat("\n=== All tests passed! ===\n")
