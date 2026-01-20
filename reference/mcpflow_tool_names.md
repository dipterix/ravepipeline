# Extract tool names from workflows

Recursively extracts all tool names referenced in workflow jobs and
sections. Supports both single workflow objects and lists of workflows.

## Usage

``` r
mcpflow_tool_names(workflows)
```

## Arguments

- workflows:

  A single workflow object or a list of workflow objects

## Value

Character vector of unique tool names

## Examples

``` r
# Extract tools from a single workflow
wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
tools <- mcpflow_tool_names(wf)

# Extract tools from multiple workflows
workflows <- mcpflow_load_all("ravepipeline")
all_tools <- mcpflow_tool_names(workflows)
```
