# List MCP tool names from a package or directory

Lists all available MCP tool names from a package or directory. This is
a lightweight wrapper around `mcptool_load_all` that returns only the
tool names without loading full definitions.

## Usage

``` r
mcptool_list(pkg)
```

## Arguments

- pkg:

  Character. Package name or path to tools directory. If `pkg` contains
  path separators, space, or `'.'`, it is treated as a directory path.
  Otherwise, it is treated as a package name.

## Value

Character vector of tool names (in format `pkg-function_name`). Returns
empty character vector if no tools found.

## Examples

``` r
# List tools from package
mcptool_list("ravepipeline")
#> [1] "ravepipeline-mcp_configure_output_settings"         
#> [2] "ravepipeline-mcp_get_current_rave_pipeline_info"    
#> [3] "ravepipeline-mcp_get_current_rave_pipeline_progress"
#> [4] "ravepipeline-mcp_list_rave_pipelines"               
#> [5] "ravepipeline-mcp_load_rave_pipeline"                
#> [6] "ravepipeline-mcp_read_current_rave_pipeline_results"
#> [7] "ravepipeline-mcp_run_current_rave_pipeline"         
#> [8] "ravepipeline-mcp_set_current_rave_pipeline_settings"
#> [9] "ravepipeline-mcpflow_load_all"                      


# Or list tools from directory
path <- system.file("mcp", "tools", package = "ravepipeline")
mcptool_list(path)
#> [1] "ravepipeline-mcp_configure_output_settings"         
#> [2] "ravepipeline-mcp_get_current_rave_pipeline_info"    
#> [3] "ravepipeline-mcp_get_current_rave_pipeline_progress"
#> [4] "ravepipeline-mcp_list_rave_pipelines"               
#> [5] "ravepipeline-mcp_load_rave_pipeline"                
#> [6] "ravepipeline-mcp_read_current_rave_pipeline_results"
#> [7] "ravepipeline-mcp_run_current_rave_pipeline"         
#> [8] "ravepipeline-mcp_set_current_rave_pipeline_settings"
#> [9] "ravepipeline-mcpflow_load_all"                      
```
