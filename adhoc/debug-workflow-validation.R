# Debug workflow validation

library(yaml)

workflow_dir <- "inst/mcp/workflows"
yaml_files <- list.files(workflow_dir, pattern = "\\.yaml$", full.names = TRUE)

cat("Found", length(yaml_files), "workflow files\n")

for (yaml_file in yaml_files) {
  cat("\nProcessing:", yaml_file, "\n")
  
  workflow <- tryCatch({
    wf <- yaml::read_yaml(yaml_file)
    cat("  Type:", class(wf), "\n")
    cat("  Is list:", is.list(wf), "\n")
    if (is.list(wf)) {
      cat("  Has name:", !is.null(wf$name), "\n")
      if (!is.null(wf$name)) {
        cat("  Name value:", wf$name, "\n")
      }
    }
    wf
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    NULL
  })
}
