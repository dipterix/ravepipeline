# Load Tools for Workflow

Loads and returns the MCP tools referenced by a workflow.

## Usage

``` r
mcpflow_tools(workflow, tools_dir = NULL)
```

## Arguments

- workflow:

  A workflow object

- tools_dir:

  Character. Base directory containing `tools/` subdirectory. Default is
  `NULL`, which uses the `'source_tools_dir'` attribute from the
  workflow or falls back to `"./tools"`.

## Value

A named list of tool definitions
