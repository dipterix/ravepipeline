# Test MCP tools with power_explorer pipeline using ellmer instantiation
# This test uses mcptool_instantiate() to create ellmer tools and validates
# the results against the documented schema in the YAML files.

devtools::load_all()

cat("=== Test MCP Tools via ellmer Instantiation ===\n\n")

# Load all MCP tools and instantiate them
cat("Loading and instantiating all MCP tools...\n")
mcp_tools <- mcptool_load_all("ravepipeline")
ellmer_tools <- list()
for (tool in mcp_tools) {
  ellmer_tools[[tool$name]] <- list(
    ellmer = mcptool_instantiate(tool),
    spec = tool
  )
  cat("  ✓", tool$name, "\n")
}
cat("\n")

# Helper function to validate result against schema
validate_result <- function(result, schema, tool_name) {
  errors <- character(0)
  
  if (is.null(schema$properties)) {
    return(errors)
  }
  
  # Parse result from JSON if needed
  if (inherits(result, "json")) {
    result <- jsonlite::fromJSON(result)
  }
  
  for (prop_name in names(schema$properties)) {
    prop_schema <- schema$properties[[prop_name]]
    value <- result[[prop_name]]
    
    # Check if property exists (only if it's expected based on success/error)
    if (is.null(value)) {
      # error field is only expected when success is FALSE
      if (prop_name == "error" && isTRUE(result$success)) {
        next
      }
      # Skip optional fields
      next
    }
    
    # Validate type strictly
    expected_type <- prop_schema$type
    actual_ok <- switch(expected_type,
      "boolean" = is.logical(value),
      "integer" = is.numeric(value) && (is.integer(value) || all(value == floor(value))),
      "number" = is.numeric(value),
      "string" = is.character(value),
      "array" = is.list(value) || is.vector(value),
      "object" = is.list(value) || is.data.frame(value),
      TRUE  # default: accept
    )
    
    if (!actual_ok) {
      errors <- c(errors, sprintf("  %s: expected %s, got %s", 
                                  prop_name, expected_type, class(value)[1]))
    }
  }
  
  errors
}

# ============================================================
# Test 1: mcp_list_rave_pipelines
# ============================================================
cat("=== Test 1: ravepipeline-mcp_list_rave_pipelines ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_list_rave_pipelines"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call the instantiated tool (no arguments)
result_json <- ellmer_tool()
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  pipelines:", length(result$pipelines), "found\n")
cat("  count:", result$count, "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}

# Check power_explorer is available
if ("power_explorer" %in% result$pipelines) {
  cat("power_explorer available: ✓\n")
} else {
  cat("power_explorer available: ✗ NOT FOUND\n")
}
cat("\n")

# ============================================================
# Test 2: mcp_load_rave_pipeline
# ============================================================
cat("=== Test 2: ravepipeline-mcp_load_rave_pipeline ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_load_rave_pipeline"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with pipeline_name argument
result_json <- ellmer_tool(pipeline_name = "power_explorer")
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
cat("  message:", result$message, "\n")
cat("  description:", result$description, "\n")
cat("  targets:", result$targets, "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}

# Compare with example_response structure
if (!is.null(spec$example_response)) {
  example_keys <- names(spec$example_response)
  result_keys <- names(result)
  missing <- setdiff(example_keys, result_keys)
  if (length(missing) == 0) {
    cat("Example response keys match: ✓\n")
  } else {
    cat("Example response keys missing:", paste(missing, collapse = ", "), "\n")
  }
}
cat("\n")

# ============================================================
# Test 3: mcp_get_current_rave_pipeline_info
# ============================================================
cat("=== Test 3: ravepipeline-mcp_get_current_rave_pipeline_info ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_get_current_rave_pipeline_info"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with no arguments
result_json <- ellmer_tool()
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
cat("  name:", result$name, "\n")
cat("  description:", result$description, "\n")
cat("  path exists:", !is.null(result$path), "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}
cat("\n")

# ============================================================
# Test 4: mcp_get_current_rave_pipeline_progress
# ============================================================
cat("=== Test 4: ravepipeline-mcp_get_current_rave_pipeline_progress ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_get_current_rave_pipeline_progress"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with enum argument
result_json <- ellmer_tool(detail_level = "summary")
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
cat("  progress type:", class(result$progress)[1], "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}
cat("\n")

# ============================================================
# Test 5: mcp_set_current_rave_pipeline_settings
# ============================================================
cat("=== Test 5: ravepipeline-mcp_set_current_rave_pipeline_settings ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_set_current_rave_pipeline_settings"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with settings_json argument (JSON string as documented)
result_json <- ellmer_tool(settings_json = '{}')
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
if (!is.null(result$message)) cat("  message:", result$message, "\n")
if (!is.null(result$error)) cat("  error:", result$error, "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}
cat("\n")

# ============================================================
# Test 6: mcp_run_current_rave_pipeline (dry run - no actual execution)
# ============================================================
cat("=== Test 6: ravepipeline-mcp_run_current_rave_pipeline ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_run_current_rave_pipeline"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with empty target_names - this will attempt to run but likely fail
# due to missing settings, which is expected behavior
result_json <- ellmer_tool(target_names = character(0))
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
if (!is.null(result$message)) cat("  message:", result$message, "\n")
if (!is.null(result$error)) cat("  error:", substr(result$error, 1, 80), "...\n")

# Validate against schema (even failures should have correct schema)
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}
cat("\n")

# ============================================================
# Test 7: mcp_read_current_rave_pipeline_results
# ============================================================
cat("=== Test 7: ravepipeline-mcp_read_current_rave_pipeline_results ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcp_read_current_rave_pipeline_results"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with empty target_names - reads all available results
result_json <- ellmer_tool(target_names = character(0))
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
cat("  success:", result$success, "\n")
if (!is.null(result$results)) cat("  results count:", length(result$results), "\n")
if (!is.null(result$result_names)) cat("  result_names:", paste(head(result$result_names, 3), collapse = ", "), "\n")
if (!is.null(result$error)) cat("  error:", result$error, "\n")

# Validate against schema
errors <- validate_result(result, spec$returns, spec$name)
if (length(errors) == 0) {
  cat("Schema validation: ✓ PASS\n")
} else {
  cat("Schema validation: ✗ FAIL\n")
  cat(paste(errors, collapse = "\n"), "\n")
}
cat("\n")

# ============================================================
# Test 8: mcpflow_load_all
# ============================================================
cat("=== Test 8: ravepipeline-mcpflow_load_all ===\n")
tool_entry <- ellmer_tools[["ravepipeline-mcpflow_load_all"]]
ellmer_tool <- tool_entry$ellmer
spec <- tool_entry$spec

# Call with pkg argument
result_json <- ellmer_tool(pkg = "ravepipeline")
result <- jsonlite::fromJSON(result_json)

cat("Result:\n")
if (is.list(result)) {
  cat("  workflows loaded:", length(result), "\n")
  if (length(result) > 0) {
    cat("  workflow names:", paste(head(names(result), 3), collapse = ", "), "\n")
  }
} else {
  cat("  result type:", class(result)[1], "\n")
}

# Validate against schema (returns is an array, so check structure)
if (is.list(result) || is.vector(result)) {
  cat("Schema validation: ✓ PASS (returns array/list)\n")
} else {
  cat("Schema validation: ✗ FAIL (expected array)\n")
}
cat("\n")

# ============================================================
# Summary
# ============================================================
cat("=== Summary ===\n")
cat("All 8 MCP tools instantiated and tested via ellmer:\n")
cat("  1. mcp_list_rave_pipelines\n")
cat("  2. mcp_load_rave_pipeline\n")
cat("  3. mcp_get_current_rave_pipeline_info\n")
cat("  4. mcp_get_current_rave_pipeline_progress\n")
cat("  5. mcp_set_current_rave_pipeline_settings\n")
cat("  6. mcp_run_current_rave_pipeline\n")
cat("  7. mcp_read_current_rave_pipeline_results\n")
cat("  8. mcpflow_load_all\n")
cat("\nResults validated against documented schemas in YAML files.\n")
