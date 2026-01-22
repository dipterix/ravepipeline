# Instantiate from RAVE MCP tool definition

Create an `ellmer` tool object from a loaded RAVE MCP tool definition.
This allows using RAVE pipelines and tools directly via the `ellmer`
package.

## Usage

``` r
mcptool_instantiate(tool, ..., state_env = NULL)
```

## Arguments

- tool:

  An object of class `ravepipeline_mcp_tool` (loaded via
  [`mcptool_read`](http://dipterix.org/ravepipeline/reference/mcptool_read.md)
  or
  [`mcptool_load_all`](http://dipterix.org/ravepipeline/reference/mcptool_load_all.md)).

- ...:

  Additional arguments passed to
  [`tool`](https://ellmer.tidyverse.org/reference/tool.html).

- state_env:

  Environment or `NULL` (default). Environment in which the MCP tools
  share and store data; see
  [`mcptool_state_factory`](http://dipterix.org/ravepipeline/reference/mcptool_state_factory.md)

## Value

An [`ToolDef`](https://ellmer.tidyverse.org/reference/tool.html) object
ready to be registered with a chat session.
