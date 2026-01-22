# Create workflow guidance tool

Creates an `'ellmer'` tool that provides on-demand access to workflow
documentation sections. This is used internally by
[`mcpflow_instantiate`](http://dipterix.org/ravepipeline/reference/mcpflow_instantiate.md)
when a workflow has `prompt_sections` defined, allowing the AI to fetch
detailed documentation only when needed to reduce initial system prompt
size.

## Usage

``` r
mcpflow_guidance_tool(workflow, sections)
```

## Arguments

- workflow:

  A `'ravepipeline_mcp_workflow'` object.

- sections:

  Character vector of available section names.

## Value

An `'ellmer'` tool object.
