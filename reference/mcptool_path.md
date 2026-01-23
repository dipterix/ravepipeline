# Find path to a specific MCP tool by name

Locates an MCP tool YAML file from either an installed package or a
local directory. Tool names use `pkg-function_name` format where the
first hyphen separates the package name from the function name. Returns
the file path with the tool definition attached as an attribute.

## Usage

``` r
mcptool_path(tool_name, tools_dir = "../tools")
```

## Arguments

- tool_name:

  Character. Tool name in format `pkg-function_name`. The first hyphen
  separates package name from function name (function names may contain
  hyphens).

- tools_dir:

  Character. Path to root directory for local tools look-up. Default is
  `"../tools"` (relative to workflows directory).

## Value

Character string containing the path to the tool YAML file, with
attribute `mcp_definition` containing the parsed tool definition. Throws
an error if tool not found.

## Examples

``` r
# Find tool from package
tool_path <- mcptool_path("ravepipeline-mcp_tool_pipeline_list")
attr(tool_path, "mcp_definition")
#> RAVE MCP Tool: ravepipeline-mcp_tool_pipeline_list
#> Parameters: 0
#> Category: discovery 


if (FALSE) { # \dontrun{

# Simple tool name (looks in tools_dir only)
tool_path <- mcptool_path("my_tool", tools_dir = "./tools")

} # }
```
