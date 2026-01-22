# Convert Workflow to Markdown

Converts a workflow `YAML` object to formatted `Markdown`

## Usage

``` r
convert_workflow_to_markdown(workflow, sections = NULL)
```

## Arguments

- workflow:

  A MCP workflow object

- sections:

  Character vector of section names to include, or `NULL` to include all
  sections. Valid section names are: `"overview"`, `"tool_guide"`,
  `"warnings"`, `"coding_guidelines"`, `"code_generation_rules"`,
  `"best_practices"`, `"jobs"`, `"examples"`. Header, description,
  metadata, MCP tools, and settings are always included regardless of
  this parameter.

## Value

Character vector of markdown lines
