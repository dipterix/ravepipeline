# List RAVE MCP workflow names

Lists all available MCP workflow names from a package or directory.
Returns workflow names with source information (path and package) as
attributes.

## Usage

``` r
mcpflow_list(pkg, path = system.file("mcp", "workflows", package = pkg))
```

## Arguments

- pkg:

  Character. Package name. If missing, must specify path directly

- path:

  Character. Path to workflows directory. Default is
  `system.file("mcp", "workflows", package = pkg)`

## Value

Character vector of workflow names with attribute `"source_from"`
containing a list with `"path"` and `"pkg"` (if applicable)

## Examples

``` r

# List workflows from package
workflows <- mcpflow_list("ravepipeline")

# Get source information
attr(workflows, "source_from")
#> $path
#> [1] "/home/runner/work/_temp/Library/ravepipeline/mcp/workflows"
#> 
#> $pkg
#> [1] "ravepipeline"
#> 


# List from custom path
path <- system.file("mcp", "workflows",
                     package = "ravepipeline")

mcpflow_list(path = path)
#> [1] "rave_pipeline_class_guide"
#> attr(,"source_from")
#> attr(,"source_from")$path
#> [1] "/home/runner/work/_temp/Library/ravepipeline/mcp/workflows"
#> 
#> attr(,"source_from")$pkg
#> NULL
#> 
```
