# List MCP tool names from a package or directory

Lists all available MCP tool names from a package or directory. This is
a lightweight wrapper around `mcptool_load_all` that returns only the
tool names without loading full definitions.

## Usage

``` r
mcptool_list(pkg, groups = NULL)
```

## Arguments

- pkg:

  Character. Package name or path to tools directory. If `pkg` contains
  path separators, space, or `'.'`, it is treated as a directory path.
  Otherwise, it is treated as a package name.

- groups:

  Character vector or NULL. Optional group filter(s) to list only tools
  from specific group(s). Groups are extracted from tool names following
  the pattern `pkg-mcp_tool_{group}_{action}`. Each group can be a
  regexp pattern. If `NULL` (default), all tools are listed.

## Value

Character vector of tool names (in format `pkg-function_name`). Returns
empty character vector if no tools found.

## Examples

``` r
# List all tools from package
mcptool_list("ravepipeline")
#>  [1] "ravepipeline-mcp_tool_config_set_outputs"              
#>  [2] "ravepipeline-mcp_tool_docs_available_vignettes"        
#>  [3] "ravepipeline-mcp_tool_docs_help_page"                  
#>  [4] "ravepipeline-mcp_tool_docs_package_help_topics"        
#>  [5] "ravepipeline-mcp_tool_docs_vignette"                   
#>  [6] "ravepipeline-mcp_tool_pipeline_get_helpers"            
#>  [7] "ravepipeline-mcp_tool_pipeline_get_info"               
#>  [8] "ravepipeline-mcp_tool_pipeline_get_progress"           
#>  [9] "ravepipeline-mcp_tool_pipeline_get_script"             
#> [10] "ravepipeline-mcp_tool_pipeline_get_target_dependencies"
#> [11] "ravepipeline-mcp_tool_pipeline_list"                   
#> [12] "ravepipeline-mcp_tool_pipeline_load"                   
#> [13] "ravepipeline-mcp_tool_pipeline_read_results"           
#> [14] "ravepipeline-mcp_tool_pipeline_run"                    
#> [15] "ravepipeline-mcp_tool_pipeline_set_settings"           
#> [16] "ravepipeline-mcp_tool_run_r"                           
#> [17] "ravepipeline-mcp_tool_search_package_info"             
#> [18] "ravepipeline-mcp_tool_search_packages"                 

# List only pipeline group tools
mcptool_list("ravepipeline", groups = "pipeline")
#>  [1] "ravepipeline-mcp_tool_pipeline_get_helpers"            
#>  [2] "ravepipeline-mcp_tool_pipeline_get_info"               
#>  [3] "ravepipeline-mcp_tool_pipeline_get_progress"           
#>  [4] "ravepipeline-mcp_tool_pipeline_get_script"             
#>  [5] "ravepipeline-mcp_tool_pipeline_get_target_dependencies"
#>  [6] "ravepipeline-mcp_tool_pipeline_list"                   
#>  [7] "ravepipeline-mcp_tool_pipeline_load"                   
#>  [8] "ravepipeline-mcp_tool_pipeline_read_results"           
#>  [9] "ravepipeline-mcp_tool_pipeline_run"                    
#> [10] "ravepipeline-mcp_tool_pipeline_set_settings"           

# List multiple groups
mcptool_list("ravepipeline", groups = c("pipeline", "config"))
#>  [1] "ravepipeline-mcp_tool_config_set_outputs"              
#>  [2] "ravepipeline-mcp_tool_pipeline_get_helpers"            
#>  [3] "ravepipeline-mcp_tool_pipeline_get_info"               
#>  [4] "ravepipeline-mcp_tool_pipeline_get_progress"           
#>  [5] "ravepipeline-mcp_tool_pipeline_get_script"             
#>  [6] "ravepipeline-mcp_tool_pipeline_get_target_dependencies"
#>  [7] "ravepipeline-mcp_tool_pipeline_list"                   
#>  [8] "ravepipeline-mcp_tool_pipeline_load"                   
#>  [9] "ravepipeline-mcp_tool_pipeline_read_results"           
#> [10] "ravepipeline-mcp_tool_pipeline_run"                    
#> [11] "ravepipeline-mcp_tool_pipeline_set_settings"           

# Or list tools from directory
path <- system.file("mcp", "tools", package = "ravepipeline")
mcptool_list(path)
#>  [1] "ravepipeline-mcp_tool_config_set_outputs"              
#>  [2] "ravepipeline-mcp_tool_docs_available_vignettes"        
#>  [3] "ravepipeline-mcp_tool_docs_help_page"                  
#>  [4] "ravepipeline-mcp_tool_docs_package_help_topics"        
#>  [5] "ravepipeline-mcp_tool_docs_vignette"                   
#>  [6] "ravepipeline-mcp_tool_pipeline_get_helpers"            
#>  [7] "ravepipeline-mcp_tool_pipeline_get_info"               
#>  [8] "ravepipeline-mcp_tool_pipeline_get_progress"           
#>  [9] "ravepipeline-mcp_tool_pipeline_get_script"             
#> [10] "ravepipeline-mcp_tool_pipeline_get_target_dependencies"
#> [11] "ravepipeline-mcp_tool_pipeline_list"                   
#> [12] "ravepipeline-mcp_tool_pipeline_load"                   
#> [13] "ravepipeline-mcp_tool_pipeline_read_results"           
#> [14] "ravepipeline-mcp_tool_pipeline_run"                    
#> [15] "ravepipeline-mcp_tool_pipeline_set_settings"           
#> [16] "ravepipeline-mcp_tool_run_r"                           
#> [17] "ravepipeline-mcp_tool_search_package_info"             
#> [18] "ravepipeline-mcp_tool_search_packages"                 
```
