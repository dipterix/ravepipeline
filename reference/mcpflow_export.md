# Export RAVE MCP workflows

Saves workflow objects to `YAML` and/or `Markdown` format. Optionally
exports associated MCP tools to the same directory. Workflows are saved
to `output_dir/workflows/` and tools to `output_dir/tools/`.

## Usage

``` r
mcpflow_export(
  workflows,
  output_dir,
  format = c("yaml", "markdown", "both"),
  save_tools = TRUE,
  verbose = TRUE
)
```

## Arguments

- workflows:

  Either a `"ravepipeline_mcp_workflow"` object, a list of
  `"ravepipeline_mcp_workflow"` objects, or a character string path to a
  workflow `YAML` file

- output_dir:

  Character string, base directory to save output files. If `NULL`
  (default), uses the parent directory of the input file's folder (when
  workflows is a path) or current working directory (when workflows is
  an object). Workflows are saved to `output_dir/workflows/`, tools to
  `output_dir/tools/`

- format:

  Character string, output format. Options: `"markdown"` (default),
  `"yaml"`, `"both"`

- save_tools:

  Logical, whether to save referenced MCP tools to `output_dir/tools/`;
  default is `TRUE`

- verbose:

  Logical, whether to print progress messages; default is `TRUE`

## Value

Invisibly returns a character vector of paths to written files

## Examples

``` r
# Load workflows from package and export to temp directory
workflows <- mcpflow_load_all("ravepipeline")

temp_dir <- tempfile("mcp_export")
written_files <- mcpflow_export(
  workflows,
  output_dir = temp_dir,
  format = "both",
  save_tools = TRUE,
  verbose = FALSE
)

# Show exported files
list.files(temp_dir, recursive = TRUE)
#>  [1] "tools/ravepipeline-mcp_configure_output_settings.yaml"         
#>  [2] "tools/ravepipeline-mcp_get_current_rave_pipeline_info.yaml"    
#>  [3] "tools/ravepipeline-mcp_get_current_rave_pipeline_progress.yaml"
#>  [4] "tools/ravepipeline-mcp_list_rave_pipelines.yaml"               
#>  [5] "tools/ravepipeline-mcp_load_rave_pipeline.yaml"                
#>  [6] "tools/ravepipeline-mcp_read_current_rave_pipeline_results.yaml"
#>  [7] "tools/ravepipeline-mcp_run_current_rave_pipeline.yaml"         
#>  [8] "tools/ravepipeline-mcp_set_current_rave_pipeline_settings.yaml"
#>  [9] "workflows/rave_pipeline_class_guide.md"                        
#> [10] "workflows/rave_pipeline_class_guide.yaml"                      

# Cleanup
unlink(temp_dir, recursive = TRUE)
```
