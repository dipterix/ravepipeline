# Find path to RAVE MCP workflow

Extracts the file path for a workflow by name from `mcpflow_list`
results.

## Usage

``` r
mcpflow_path(
  workflow_name,
  pkg,
  path = system.file("mcp", "workflows", package = pkg)
)
```

## Arguments

- workflow_name:

  Character. Workflow name to find

- pkg:

  Character. Package name. If NULL, uses path parameter

- path:

  Character. Path to workflows directory. If `pkg` is provided, uses
  `mcp/workflows` under the package folder

## Value

Character string with the path to the workflow YAML file. Throws an
error if workflow not found.

## Examples

``` r
# Find workflow path from package
wf_path <- mcpflow_path("rave_pipeline_class_guide", pkg = "ravepipeline")
wf_path
#> [1] "/home/runner/work/_temp/Library/ravepipeline/mcp/workflows/rave_pipeline_class_guide.yaml"
```
