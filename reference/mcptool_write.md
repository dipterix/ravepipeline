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
path <- mcptool_path("ravepipeline-mcp_tool_pipeline_list")
tool <- mcptool_read(path)

# Write as YAML to temporary file
mcptool_write(tool, stdout(), method = "yaml")
#> name: ravepipeline-mcp_tool_pipeline_list
#> description: ~
#> parameters:
#>   type: object
#>   properties: {}
#> category: discovery

# Write as Markdown
mcptool_write(tool, stdout(), method = "markdown")
#> # ravepipeline-mcp_tool_pipeline_list
#> 
#> ## Parameters
#> 
#> ## Metadata
#> 
#> - **Category**: discovery
#> 
```
