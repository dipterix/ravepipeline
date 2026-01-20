# Load RAVE MCP workflows

Loads workflow `YAML` files from a package's MCP workflows directory and
returns them as `S3` objects with class `"ravepipeline_mcp_workflow"`.

## Usage

``` r
mcpflow_load_all(pkg, path = system.file("mcp", package = pkg))
```

## Arguments

- pkg:

  Character string. Package name to load workflows from. Example values:
  `"ravepipeline"`

- path:

  Character string. Optional path to workflows directory. If provided,
  loads from this path instead of package `mcp/workflows` directory.

## Value

A structured named list of workflow objects, or an empty list if no
workflows found.

## Examples

``` r
# MCP example response:
# {
#   "rave_pipeline_class_guide": {
#     "name": "rave_pipeline_class_guide",
#     "description": "...",
#     "mcp_tools": true
#   }
# }

workflow <- mcpflow_load_all(pkg = "ravepipeline")
```
