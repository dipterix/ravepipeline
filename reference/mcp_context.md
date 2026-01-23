# Add Context to Chat Messages

Formats R objects and combines them with optional text to provide
context for chat messages. Similar to `btw::btw()`, this function wraps
formatted output in a structured format that chat models can easily
understand.

This function uses
[`mcp_describe`](http://dipterix.org/ravepipeline/reference/mcp_describe.md)
internally to format objects into human-readable text, then wraps the
output in code blocks and returns an
[`ellmer::ContentText`](https://ellmer.tidyverse.org/reference/Content.html)
object suitable for use with `ellmer::Chat$chat()`.

## Usage

``` r
mcp_context(..., .header = "## Context", .wrap_code = TRUE)
```

## Arguments

- ...:

  R objects to format, or character strings to include. Each argument
  will be formatted using
  [`mcp_describe()`](http://dipterix.org/ravepipeline/reference/mcp_describe.md)
  unless it's a plain text string.

- .header:

  Character string for the context header. Default: `"## Context"`

- .wrap_code:

  Logical. Whether to wrap non-text objects in code blocks. Default:
  `TRUE`

## Value

An
[`ellmer::ContentText`](https://ellmer.tidyverse.org/reference/Content.html)
object containing the formatted context. This object can be passed
directly to `chat$chat()`.

## Details

The function processes each argument as follows:

- Package references like `"{packagename}"` are expanded to show all
  help topics from that package in JSON format

- Plain text strings (single character vectors) are used as-is

- Other R objects are formatted using
  [`mcp_describe()`](http://dipterix.org/ravepipeline/reference/mcp_describe.md)

- Formatted objects are wrapped in code blocks if `.wrap_code = TRUE`

- All parts are combined with the header into a single text block

The resulting `ContentText` object is compatible with `ellmer` chat
functions and will be automatically processed when passed to
`chat$chat()`.

## See also

[`mcp_describe`](http://dipterix.org/ravepipeline/reference/mcp_describe.md)
for the underlying formatting function,
[`mcpflow_instantiate`](http://dipterix.org/ravepipeline/reference/mcpflow_instantiate.md)
for workflow integration

## Examples

``` r
if (FALSE) { # \dontrun{
# Single object
mcp_context(sessionInfo())

# Package help topics
mcp_context("{ravepipeline}")

# Multiple objects with question
mcp_context(sessionInfo(), mtcars, "What packages and data are available?")

# Use in chat
chat <- ellmer::chat_anthropic()
response <- chat$chat(
  mcp_context(sessionInfo(), "What R version am I using?")
)

# Combine with workflow instantiation
wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
chat <- mcpflow_instantiate(wf, chat = chat)
response <- chat$chat(
  mcp_context(
    sessionInfo(),
    "Generate code for power analysis"
  )
)
} # }
```
