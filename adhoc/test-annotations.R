#!/usr/bin/env Rscript

devtools::load_all(".")

tool <- ravepipeline::mcptool_instantiate(ravepipeline::mcptool_read(system.file(
  "mcp", "tools", "ravepipeline-mcp_list_rave_pipelines.yaml",
  package = "ravepipeline"
)))

cat("=== Tool structure ===\n")
cat("Name:", tool@name, "\n")
cat("Description:", substr(tool@description, 1, 50), "...\n\n")

cat("=== Annotations class ===\n")
print(class(tool@annotations))

cat("\n=== Annotation names ===\n")
print(names(tool@annotations))

if (length(tool@annotations) > 0) {
  cat("\n=== Custom annotation values ===\n")
  if (!is.null(tool@annotations$category)) {
    cat("Category:", tool@annotations$category, "\n")
  }
  if (!is.null(tool@annotations$execution_time)) {
    cat("Execution time:", tool@annotations$execution_time, "seconds\n")
  }
  if (!is.null(tool@annotations$implementation_example)) {
    cat("\nImplementation example (first 150 chars):\n")
    cat(substr(tool@annotations$implementation_example, 1, 150), "...\n")
  }
  if (!is.null(tool@annotations$returns)) {
    cat("\nReturns type:", tool@annotations$returns$type, "\n")
  }
} else {
  cat("\n[No annotations found]\n")
}

