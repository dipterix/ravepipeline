# Test workflow validation in mcptools_build

library(ravepipeline)

cat("=== Building MCP Tools ===\n")
devtools::load_all(".")
mcptools_build()

cat("\n=== Done ===\n")
