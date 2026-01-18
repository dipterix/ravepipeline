# Test that all MCP tools can be instantiated with ellmer
# This verifies the tool definitions are compatible with ellmer::tool()

devtools::load_all()

cat("=== Test: Load all MCP tools ===\n")
# Load from the package tools directory
tools <- mcptool_load_all("ravepipeline")
cat("Loaded", length(tools), "tools:\n")
for (tool in tools) {
  cat("  -", tool$name, "\n")
}

cat("\n=== Test: Instantiate each tool with ellmer ===\n")
instantiated_tools <- list()
errors <- list()

for (tool in tools) {
  cat("Instantiating:", tool$name, "... ")
  result <- tryCatch({
    ellmer_tool <- mcptool_instantiate(tool)
    instantiated_tools[[tool$name]] <- ellmer_tool
    cat("OK\n")
    TRUE
  }, error = function(e) {
    cat("FAILED:", conditionMessage(e), "\n")
    errors[[tool$name]] <- conditionMessage(e)
    FALSE
  })
}

cat("\n=== Test: Verify ellmer tool properties ===\n")
for (name in names(instantiated_tools)) {
  ellmer_tool <- instantiated_tools[[name]]
  cat("\nTool:", name, "\n")
  
  # Check it's a ToolDef object (S7 reports class as "ellmer::ToolDef")
  tool_classes <- class(ellmer_tool)
  is_tooldef <- any(grepl("ToolDef", tool_classes))
  if (!is_tooldef) {
    cat("  ERROR: Not a ToolDef object, class:", paste(tool_classes, collapse = ", "), "\n")
    errors[[name]] <- "Not a ToolDef object"
    next
  }
  
  # Check name format (should only contain allowed characters)
  # ellmer uses S7, so use @ instead of $
  tool_name <- ellmer_tool@name
  if (!grepl("^[a-zA-Z0-9_-]+$", tool_name)) {
    cat("  ERROR: Invalid tool name format:", tool_name, "\n")
    errors[[name]] <- paste("Invalid name format:", tool_name)
    next
  }
  cat("  Name:", tool_name, "✓\n")
  
  # Check description exists
  tool_desc <- ellmer_tool@description
  if (is.null(tool_desc) || nchar(tool_desc) == 0) {
    cat("  WARNING: Empty description\n")
  } else {
    cat("  Description: [", nchar(tool_desc), "chars ] ✓\n")
  }
  
  # Check it's callable (ToolDef inherits from function)
  if (!is.function(ellmer_tool)) {
    cat("  ERROR: Not callable\n")
    errors[[name]] <- "Not callable"
    next
  }
  cat("  Callable: TRUE ✓\n")
  
  # Check arguments/schema
  tool_args <- ellmer_tool@arguments
  if (!is.null(tool_args)) {
    # arguments is a TypeObject with @properties
    n_args <- length(tool_args@properties)
    cat("  Arguments:", n_args, "params ✓\n")
  }
}

cat("\n=== Test: Verify tool name format compliance ===\n")
# ellmer only allows: letters, digits, _, or -
for (name in names(instantiated_tools)) {
  ellmer_tool <- instantiated_tools[[name]]
  if (!grepl("^[a-zA-Z0-9_-]+$", ellmer_tool@name)) {
    cat("FAIL:", name, "has invalid characters in name:", ellmer_tool@name, "\n")
    errors[[name]] <- paste("Invalid name:", ellmer_tool@name)
  } else {
    cat("PASS:", ellmer_tool@name, "\n")
  }
}

cat("\n=== Summary ===\n")
cat("Total tools:", length(tools), "\n")
cat("Successfully instantiated:", length(instantiated_tools), "\n")
cat("Errors:", length(errors), "\n")

if (length(errors) > 0) {
  cat("\nErrors:\n")
  for (name in names(errors)) {
    cat("  -", name, ":", errors[[name]], "\n")
  }
  stop("Some tools failed to instantiate correctly")
} else {
  cat("\nAll tools instantiated successfully! ✓\n")
}
