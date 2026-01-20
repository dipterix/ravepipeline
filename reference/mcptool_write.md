# Write MCP tool definition to file

Writes an MCP tool definition to a file in either `YAML` or `Markdown`
format.

## Usage

``` r
mcptool_write(tool, path, method = c("yaml", "markdown"), ...)
```

## Arguments

- tool:

  A `ravepipeline_mcp_tool` object

- path:

  Character. Path to the output file

- method:

  Character. Output format: `"yaml"` (default) or `"markdown"`

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input tool object

## Examples

``` r
# Load a tool from package
path <- mcptool_path("ravepipeline-mcp_list_rave_pipelines")
tool <- mcptool_read(path)

# Write as YAML to temporary file
mcptool_write(tool, stdout(), method = "yaml")
#> name: ravepipeline-mcp_list_rave_pipelines
#> description: List all installed RAVE pipelines available on the system.
#> parameters:
#>   type: object
#>   properties: {}
#> category: discovery
#> example_response:
#>   pipelines:
#>   - block_explorer
#>   - custom_3d_viewer
#>   - generate_surface_atlas
#>   - group_3d_viewer
#>   - import_bids
#>   - import_lfp_native
#>   - import_signals
#>   - notch_filter
#>   - power_clust
#>   - power_explorer
#>   - project_overview
#>   - reference_module
#>   - stimpulse_finder
#>   - surface_reconstruction
#>   - trace_viewer
#>   - voltage_clust
#>   - wavelet_module
#>   - yael_preprocess
#>   count: 18
#> returns:
#>   type: object
#>   properties:
#>     count:
#>       type: integer
#>       description: Integer, total number of available pipelines
#>     pipelines:
#>       type: array
#>       description: Character vector, pipeline names
#>       items:
#>         type: string

# Write as Markdown
mcptool_write(tool, stdout(), method = "markdown")
#> # ravepipeline-mcp_list_rave_pipelines
#> 
#> List all installed RAVE pipelines available on the system.
#> 
#> ## Parameters
#> 
#> ## Returns
#> 
#> Type: `object`
#> 
#> ### Properties
#> 
#> - **count** (integer)
#>   Integer, total number of available pipelines
#> 
#> - **pipelines** (array)
#>   Character vector, pipeline names
#> 
#> ## Metadata
#> 
#> - **Category**: discovery
#> 
```
