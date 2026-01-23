# Read MCP tool definition from `YAML` file

Reads and parses an MCP tool definition from a `YAML` file. This is a
low-level function used internally by `mcptool_load_all` and
`mcptool_path`.

## Usage

``` r
mcptool_read(path, check = FALSE)
```

## Arguments

- path:

  Character. Path to the `YAML` file containing the tool definition.

- check:

  Logical. Whether to check if the function definition exists; default
  is `FALSE`

## Value

List containing the parsed tool definition. Throws an error if file
cannot be read or parsed.

## Examples

``` r
path <- system.file(
  "mcp", "tools", "ravepipeline-mcp_tool_pipeline_list.yaml",
  package = "ravepipeline"
)

tool_def <- mcptool_read(path)

# Check tool name and description
tool_def$name
#> [1] "ravepipeline-mcp_tool_pipeline_list"
tool_def$description
#> NULL
```
