# Test mcptool_path and enhanced mcptool_load_all
# Load package functions
devtools::load_all(".")

cat("=== Test 1: mcptool_load_all with package name ===\n")
tools_from_pkg <- mcptool_load_all("ravepipeline")
cat("Loaded", length(tools_from_pkg), "tools from package\n")
cat("Tool names:\n")
print(names(tools_from_pkg))

cat("\n=== Test 2: mcptool_load_all with directory path ===\n")
tools_from_dir <- mcptool_load_all("./inst/mcp/tools")
cat("Loaded", length(tools_from_dir), "tools from directory\n")
cat("Tool names:\n")
print(names(tools_from_dir))

cat("\n=== Test 3: mcptool_load_all with relative path ===\n")
tools_from_rel <- mcptool_load_all("inst/mcp/tools")
cat("Loaded", length(tools_from_rel), "tools from relative path\n")

cat("\n=== Test 4: mcptool_path with ::: notation ===\n")
if (length(tools_from_pkg) > 0) {
  first_tool_name <- names(tools_from_pkg)[1]
  cat("Looking for tool:", first_tool_name, "\n")
  
  tool1_path <- mcptool_path(first_tool_name)
  tool1 <- attr(tool1_path, "mcp_definition")
  cat("Found tool at path:", tool1_path, "\n")
  cat("  Name:", tool1$name, "\n")
  cat("  Description:", substr(tool1$description, 1, 60), "...\n")
  cat("  Parameters:", length(tool1$parameters$properties), "\n")
}

cat("\n=== Test 5: mcptool_path with :: notation (should work identically) ===\n")
if (length(tools_from_pkg) > 0) {
  # Convert ::: to :: for testing
  test_name <- gsub(":::", "::", first_tool_name, fixed = TRUE)
  cat("Looking for tool with :: notation:", test_name, "\n")
  
  tool2_path <- mcptool_path(test_name)
  tool2 <- attr(tool2_path, "mcp_definition")
  cat("Found tool at path:", tool2_path, "\n")
  cat("  Name:", tool2$name, "\n")
  cat("  Should normalize to:", first_tool_name, "\n")
  cat("  Paths match:", identical(tool1_path, tool2_path), "\n")
  cat("  Definitions match:", identical(tool1, tool2), "\n")
}

cat("\n=== Test 6: mcptool_path with custom tools_dir ===\n")
if (length(tools_from_pkg) > 0) {
  tool3_path <- mcptool_path(first_tool_name, tools_dir = "./inst/mcp/tools")
  tool3 <- attr(tool3_path, "mcp_definition")
  cat("Found tool at path:", tool3_path, "\n")
  cat("  Name:", tool3$name, "\n")
}

cat("\n=== Test 7: mcptool_path with NULL tools_dir (package only) ===\n")
if (length(tools_from_pkg) > 0) {
  tool4_path <- mcptool_path(first_tool_name, tools_dir = NULL)
  tool4 <- attr(tool4_path, "mcp_definition")
  cat("Found tool at path:", tool4_path, "\n")
  cat("  Name:", tool4$name, "\n")
}

cat("\n=== Test 8: Error case - nonexistent tool ===\n")
tryCatch({
  bad_tool <- mcptool_path("nonexistent-fake_tool")
  cat("ERROR: Should have thrown error!\n")
}, error = function(e) {
  cat("Correctly threw error:\n")
  cat("  Message:", e$message, "\n")
})

cat("\n=== Test 9: Error case - invalid format ===\n")
tryCatch({
  bad_tool <- mcptool_path("invalid_format")
  cat("ERROR: Should have thrown error!\n")
}, error = function(e) {
  cat("Correctly threw error for invalid format:\n")
  cat("  Message:", e$message, "\n")
})

cat("\n=== Test 10: Error case - nonexistent package ===\n")
tryCatch({
  bad_tool <- mcptool_path("nonexistentpkg-some_function")
  cat("ERROR: Should have thrown error!\n")
}, error = function(e) {
  cat("Correctly threw error for nonexistent package:\n")
  cat("  Message:", e$message, "\n")
})

cat("\n=== All tests completed ===\n")
