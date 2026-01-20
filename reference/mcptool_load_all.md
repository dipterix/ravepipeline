# Load MCP Tool Definitions from Package or Directory

Load MCP Tool Definitions from Package or Directory

## Usage

``` r
mcptool_load_all(pkg)
```

## Arguments

- pkg:

  Character. Package name or path to tools directory. If `pkg` contains
  path separators, space, or `.`, it is treated as a directory path.
  Otherwise, it is treated as a package name.

## Value

List of MCP tool definitions

## Examples

``` r
# Load from package
tools <- mcptool_load_all("ravepipeline")


# Load from directory
path <- system.file("mcp", "tools", package = "ravepipeline")
tools <- mcptool_load_all(path)
```
