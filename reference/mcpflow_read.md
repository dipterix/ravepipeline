# Read RAVE MCP workflow

Reads and parses a single MCP workflow from various sources.

## Usage

``` r
mcpflow_read(x, tools = TRUE, tools_dir = NULL, ...)
```

## Arguments

- x:

  Source specification: character path, `"package::name"`, package
  namespace (environment), or workflow object

- tools:

  Logical. Whether to load referenced MCP tools. Default is TRUE

- tools_dir:

  Character. Base directory containing `tools/` directory for loading
  local tools. Default is NULL, which infers from the workflow file
  location or falls back to "./tools".

- ...:

  Additional arguments passed to methods

## Value

A workflow object with class `"ravepipeline_mcp_workflow"`

## Examples

``` r
# Read from package
wf <- mcpflow_read("ravepipeline:::rave_pipeline_class_guide")

# Read from file path
path <- system.file(
  "mcp", "workflows", "rave_pipeline_class_guide.yaml",
  package = "ravepipeline"
)
wf <- mcpflow_read(path)

```
