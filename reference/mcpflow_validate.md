# Validate RAVE MCP workflow

Validates a workflow object for required fields and structure.

## Usage

``` r
mcpflow_validate(workflow, available_tools = NULL, strict = FALSE)
```

## Arguments

- workflow:

  A RAVE workflow object or list

- available_tools:

  Character vector of available tool names. If NULL, skips tool
  reference validation

- strict:

  Logical. If `TRUE`, perform full YAML schema validation via
  `mcpflow_validate_yaml`. Default is `FALSE` for lightweight validation
  of essential fields only.

## Value

A list with components:

- valid:

  Logical, whether workflow is valid

- errors:

  Character vector of error messages (empty if valid)

- warnings:

  Character vector of warning messages

## Examples

``` r
# Read a workflow
path <- system.file(
  "mcp", "workflows", "rave_pipeline_class_guide.yaml",
  package = "ravepipeline"
)
wf <- mcpflow_read(path)

# Validate (lightweight)
result <- mcpflow_validate(wf)
result$valid
#> [1] TRUE

# Validate (strict schema validation)
result <- mcpflow_validate(wf, strict = TRUE)
result$valid
#> [1] FALSE
```
