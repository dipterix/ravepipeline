# Create validation tool for job execution

Creates a callback function that validates tool calls against workflow
job definitions. The validation tool tracks which jobs have been
completed and warns (or rejects) when tools are called out of expected
order or when job dependencies are not met.

## Usage

``` r
mcpflow_job_validator(workflow, strict = FALSE, verbose = TRUE)
```

## Arguments

- workflow:

  A MCP workflow object

- strict:

  Logical. If `TRUE`, reject out-of-order tool calls via
  [`tool_reject`](https://ellmer.tidyverse.org/reference/tool_reject.html).
  If `FALSE` (default), only warn via
  [`cli_warn`](https://cli.r-lib.org/reference/cli_abort.html).

- verbose:

  Logical. If `TRUE` (default), print progress messages when jobs and
  steps are detected.

## Value

A list with two callback functions:

- `on_tool_request`:

  Callback for `chat$on_tool_request()`

- `on_tool_result`:

  Callback for `chat$on_tool_result()`

- `state`:

  An environment containing execution state (for inspection)

## Details

The validation tool maintains state about which jobs have been
started/completed based on tool calls observed. It checks:

- Whether the tool being called matches expected job steps

- Whether job dependencies (`needs`) are satisfied

- Step execution order within jobs

## Examples

``` r
wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
validator <- mcpflow_job_validator(wf, strict = FALSE)

if (FALSE) { # \dontrun{
chat <- ellmer::chat_ollama()
chat$on_tool_request(validator$on_tool_request)
chat$on_tool_result(validator$on_tool_result)
} # }
```
