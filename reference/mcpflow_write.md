# Write RAVE MCP workflow to a file

Writes a workflow object to `YAML` and/or `Markdown` format.

## Usage

``` r
mcpflow_write(
  workflow,
  path,
  method = c("auto", "yaml", "markdown", "both"),
  ...
)
```

## Arguments

- workflow:

  A `'ravepipeline_mcp_workflow'` object

- path:

  Character. Output file path. File extension determines format (`.yaml`
  for `YAML`, `.md` for `Markdown`). If no extension, writes both
  formats

- method:

  Character. Output format: `"yaml"` (default), `"markdown"`, or
  `"both"`

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the path to written file

## Examples

``` r
# Read from a package
mcpflow_read("ravepipeline::rave_pipeline_class_guide")
#> RAVE MCP Workflow: <rave_pipeline_class_guide>
#>   Description: AI Assistant Workflows for RAVE MCP Tools - Comprehensive gu ... 
#>   Version: 1.0.0 
#>   Category: guide 
#>   MCP Tools: 8 (attached)
#>   Jobs: 7 
#>   Examples: 3 

# Read a workflow
path <- system.file(
  "mcp", "workflows", "rave_pipeline_class_guide.yaml",
  package = "ravepipeline"
)


wf <- mcpflow_read(path)

# Write as YAML to temporary file
mcpflow_write(wf, stdout(), method = "yaml")
#> name: rave_pipeline_class_guide
#> description: AI Assistant Workflows for RAVE MCP Tools - Comprehensive guide for using
#>   RAVE MCP tools to help users with neuroscience data analysis pipelines
#> version: 1.0.0
#> category: guide
#> tags:
#> - neuroscience
#> - data-analysis
#> - pipelines
#> - workflows
#> prompt_sections:
#>   core:
#>   - overview
#>   - tool_guide
#>   - warnings
#>   - coding_guidelines
#>   - code_generation_rules
#> mcp_tools:
#> - ravepipeline-mcp_list_rave_pipelines
#> - ravepipeline-mcp_load_rave_pipeline
#> - ravepipeline-mcp_get_current_rave_pipeline_info
#> - ravepipeline-mcp_set_current_rave_pipeline_settings
#> - ravepipeline-mcp_run_current_rave_pipeline
#> - ravepipeline-mcp_get_current_rave_pipeline_progress
#> - ravepipeline-mcp_read_current_rave_pipeline_results
#> - ravepipeline-mcp_configure_output_settings
#> tool_guide:
#> - tool: ravepipeline-mcp_list_rave_pipelines
#>   category: discovery
#>   when: User asks about available analyses or pipelines
#>   notes: Always start here - never assume pipeline names
#> - tool: ravepipeline-mcp_load_rave_pipeline
#>   category: discovery
#>   when: After identifying a pipeline, before any other operations
#>   notes: Required before get_info, set_settings, or run
#>   preconditions: Know the pipeline name from mcp_list_rave_pipelines
#> - tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>   category: info
#>   when: Need parameter schema, validation rules, or pipeline details
#>   notes: Use this to discover required parameters before configuration
#>   preconditions: Pipeline must be loaded
#> - tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>   category: configuration
#>   when: User provides parameter values or you need to configure analysis
#>   notes: Always validate against schema from get_info first
#>   preconditions:
#>   - Pipeline must be loaded
#>   - Parameters validated against schema
#> - tool: ravepipeline-mcp_run_current_rave_pipeline
#>   category: execution
#>   when: User confirms they want to execute the analysis
#>   dangerous: yes
#>   requires_approval: yes
#>   preconditions:
#>   - Pipeline must be loaded
#>   - Settings must be configured
#>   - User must explicitly approve execution
#> - tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>   category: monitoring
#>   when: After starting execution, to track progress
#>   notes: Poll every 5 seconds until status is 'completed'
#>   preconditions: Pipeline execution has been started
#> - tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>   category: results
#>   when: Pipeline execution completed successfully
#>   notes: |
#>     Read specific pipeline results. If unsure which result names are available,
#>     run ravepipeline-mcp_get_current_rave_pipeline_info to see the 'targets' field.
#>     WARNING: Calling without target_names extracts ALL results, which can be memory-intensive
#>     and slow for large datasets. Always specify target_names when possible.
#>   preconditions:
#>   - Pipeline execution completed
#>   - Know target names from pipeline info
#> - tool: ravepipeline-mcp_configure_output_settings
#>   category: configuration
#>   when: Need to adjust output format for large results or token optimization
#>   notes: |
#>     Controls how tool results are formatted:
#>     - JSON (structured, machine-readable) vs text (human-readable)
#>     - Automatically falls back to text for large/complex objects
#>     - Adjust thresholds based on your needs
#>   examples:
#>   - Results too large? Increase max_size_for_json_output to 102400
#>   - Want more compact output? Reduce max_print_lines to 50
#>   - Need detailed output? Increase max_print_lines to 500
#> overview: |
#>   RAVE (Reproducible Analysis and Visualization of iEEG) is primarily built
#>   with R programming language. Please use R language for reference.
#> 
#>   The RAVE-MCP tools enable AI assistants to:
#>   - Discover and explore available analysis pipelines
#>   - Configure pipeline parameters
#>   - Execute analyses with proper validation
#>   - Monitor execution progress
#>   - Retrieve and visualize results
#> best_practices:
#> - title: Always Start with Discovery
#>   do: |
#>     1. Call ravepipeline-mcp_list_rave_pipelines to find available options
#>     2. Load the relevant pipeline with ravepipeline-mcp_load_rave_pipeline
#>     3. Get parameter details with ravepipeline-mcp_get_current_rave_pipeline_info
#>     4. Present options to user with descriptions
#>   dont: Immediately try to run with guessed pipeline names or parameters
#> - title: Code Generation Workflow
#>   do: |
#>     When user asks for code:
#>     1. Call ravepipeline-mcp_list_rave_pipelines to find correct pipeline name
#>     2. Call ravepipeline-mcp_load_rave_pipeline with the correct name
#>     3. Call ravepipeline-mcp_get_current_rave_pipeline_info to get:
#>        - Available targets from targets$Names (for pipe$read())
#>        - Current settings (use as defaults)
#>        - Parameter constraints
#>     4. Generate code using ONLY methods shown in implementation_example:
#>        - pipe$read(target_name, simplify = FALSE) to read results (use 'pipe' variable, not 'pipeline')
#>        - pipe$set_settings(...) to configure
#>        - pipe$run(names = 'target', return_values = FALSE) to execute
#>     5. Use base R for plotting (plot, lines, legend) and data manipulation (aggregate, subset)
#>   dont: |
#>     - Guess pipeline names, target names, or method names
#>     - Use tidyverse packages (dplyr, ggplot2, tidyr, purrr)
#>     - Invent methods like $get_result(), $execute(), $get()
#>     - Use the magrittr pipe %>%
#>     - Use variable name 'pipeline' (use 'pipe' instead to avoid function name conflict)
#> - title: Progressive Code Generation (Discovery-First Pattern)
#>   do: |
#>     When generating R code that works with pipeline results:
#>     Step 1 - DISCOVERY PHASE (Always do this first):
#>       1. Load pipeline and run it to generate results
#>       2. Call ravepipeline-mcp_get_current_rave_pipeline_info to get targets$Names
#>       3. Read specific result: pipe$read("target_name", simplify = FALSE)
#>       4. Inspect structure: str(result_object) or dim(array_object)
#>       5. Verify FileArray dimension order, data frame columns, object classes
#> 
#>     Step 2 - CODE GENERATION PHASE (Only after discovery):
#>       1. Write code based on DISCOVERED structure, not assumptions
#>       2. Example: If str() shows array is [Frequency, Time, Trial, Electrode],
#>          write code using indices in that order: power_array[freq_idx, time_idx, trial_idx, electrode_idx]
#>       3. For FileArray objects, always use simplify = FALSE to preserve class
#> 
#>     Example workflow:
#>     ```
#>     # Discovery
#>     pipe$run(names = "power")
#>     power <- pipe$read("power", simplify = FALSE)
#>     str(power)  # Shows: num [1:100, 1:200, 1:30, 1:10] - [Freq, Time, Trial, Elec]
#> 
#>     # Now generate code with correct indices
#>     freq_idx <- which(frequencies > 70 & frequencies < 150)
#>     high_gamma <- power[freq_idx, , , ]  # Correct: freq is 1st dimension
#>     ```
#>   dont: |
#>     - Assume dimension order without checking (e.g., guessing [Time, Freq] when it's actually [Freq, Time])
#>     - Read all results with pipe$read() for discovery (use info tool instead)
#>     - Generate code before inspecting actual data structure
#>     - Simplify FileArray objects (always use simplify = FALSE)
#> - title: Validate Before Executing
#>   do: |
#>     1. Load pipeline and get parameter schema
#>     2. Validate all parameters against schema (types, ranges, enums)
#>     3. Confirm settings with user before execution
#>     4. Then execute
#>   dont: Set parameters and run without checking schema or user confirmation
#> - title: Monitor Long-Running Operations
#>   do: |
#>     1. Start pipeline execution
#>     2. Poll ravepipeline-mcp_get_current_rave_pipeline_progress every 5 seconds
#>     3. Report progress to user: "Processing: 45% complete..."
#>     4. Confirm completion: "Analysis finished successfully!"
#>   dont: Start pipeline and assume it completed without checking
#> - title: Provide Context with Results
#>   do: |
#>     Present interpreted results:
#>     "Analysis complete! Here's what I found:
#>      - Power spectrum peak at 12 Hz (alpha band)
#>      - Mean power: 15.3 µV²/Hz
#>      Would you like me to visualize these results?"
#>   dont: Return raw data dumps without interpretation
#> - title: Handle Errors Gracefully
#>   do: |
#>     Explain what went wrong and suggest fixes:
#>     "The analysis encountered an error: 'Invalid frequency range'
#>      The requested range (2-500 Hz) exceeds Nyquist frequency (500 Hz).
#>      Would you like me to rerun with 2-400 Hz instead?"
#>   dont: Just say 'Error occurred' and stop
#> - title: Required Approval Workflows
#>   do: |
#>     For dangerous operations:
#>     1. Present action details: tool, purpose, parameters, expected outcomes
#>     2. Wait for explicit user confirmation - NEVER auto-approve
#>     3. Execute only after approval
#>     4. Confirm execution started
#>   dont: Execute dangerous operations without explicit user consent
#> - title: Parameter Validation Checklist
#>   do: |
#>     Before setting parameters, validate:
#>     - Type validation: string, number, boolean, array
#>     - Enum validation: value in allowed list
#>     - Range validation: numeric bounds, string length
#>     - Required parameters: all present
#>     - Dependencies: conditional parameters valid
#>   dont: Pass parameters without validation against schema
#> - title: Output Format Optimization
#>   do: |
#>     (Typically you don't need this tool)
#>     Outputs of RAVE MCP are typically JSON. However, in some cases, the
#>     outputs might show R's direct output. This is because either some
#>     objects in the returns cannot be serialized to JSON (such as R environment
#>     or external pointers), or the object size is too large. If you read the
#>     outputs and find the object should be able to serialize to JSON but did
#>     not, adjust output settings for optimal results:
#>     1. If results fail JSON serialization because the object is simply too large: increase max_size_for_json_output
#>     2. If text output is too verbose: reduce max_print_lines
#>     3. If you need more detail: increase max_print_lines
#>     Example: mcp_configure_output_settings(max_size_for_json_output = 51200)
#>   dont: |
#>     Accept default limits when results are truncated or fail.
#>     Never execute this tool right after dangerous operations
#> anti_patterns:
#> - pattern: Using 'pipeline' as variable name
#>   wrong: |
#>     pipeline <- ravepipeline::pipeline("power_explorer")
#>   why: '''pipeline'' is both a function name and variable - creates confusion'
#>   correct: |
#>     pipe <- ravepipeline::pipeline("power_explorer")
#>     # This clearly distinguishes the instance from the function
#> - pattern: Reading all results to discover names
#>   wrong: |
#>     # Don't do this - expensive!
#>     results <- pipe$read()  # Loads ALL results into memory
#>   why: Extracts all results which can be slow and memory-intensive for large datasets
#>   correct: |
#>     # First, check what targets are available
#>     info <- ravepipeline-mcp_get_current_rave_pipeline_info
#>     # Look at info$targets to see available result names
#>     # Then read only what you need
#>     result <- pipe$read("specific_target_name", simplify = FALSE)
#> - pattern: Using non-existent pipeline methods
#>   wrong: |
#>     data <- pipe$get_result("power")     # DOES NOT EXIST
#>     pipe$execute()                       # DOES NOT EXIST
#>     pipe$run()                          # Missing return_values = FALSE
#>   why: These methods don't exist or have wrong signatures
#>   correct: |
#>     data <- pipe$read("power", simplify = FALSE)
#>     pipe$run(names = "target", return_values = FALSE)
#> - pattern: Guessing parameter or result names
#>   wrong: |
#>     pipe$set_settings(baseline = c(-1, 0))  # Guessed parameter name
#>     power <- pipe$read("high_gamma_power")  # Guessed result name
#>   why: Parameter and result names vary by pipeline - always verify first
#>   correct: |
#>     # Always call info tool first to discover actual names
#>     info <- ravepipeline-mcp_get_current_rave_pipeline_info
#>     # Check current_settings for parameter names
#>     # Check targets for result names
#>     pipe$set_settings(actual_param_name = value)
#> - pattern: Using tidyverse packages
#>   wrong: |
#>     library(dplyr)
#>     df %>% filter(Freq > 70) %>% group_by(Condition) %>% summarize(mean_power = mean(Power))
#>   why: Tidyverse not allowed unless shown in user's sessionInfo
#>   correct: |
#>     df_filtered <- df[df$Freq > 70, ]
#>     aggregate(Power ~ Condition, data = df_filtered, FUN = mean)
#> - pattern: Ignoring implementation examples
#>   wrong: |
#>     # Inventing your own pattern
#>     results <- pipe$get_data("settings")
#>   why: Implementation examples in tool definitions show the ONLY correct patterns
#>   correct: |
#>     # Follow the implementation_example exactly
#>     results <- pipe$read("settings", simplify = FALSE)
#>     # This is the canonical pattern from the tool definition
#> warnings:
#> - Never execute dangerous operations without explicit user approval
#> - Always validate parameters against schema before setting
#> - Monitor long-running pipelines - don't assume completion
#> - Handle errors gracefully with clear explanations and recovery suggestions
#> - Start with discovery tools before assuming pipeline names or parameters
#> - Provide context and interpretation with results, not just raw data
#> - 'For dangerous: true operations, show WARNING and request double confirmation'
#> coding_guidelines:
#> - 'Package Usage Restrictions: If the user provides session information, ONLY use
#>   packages listed as attached. Otherwise, the ONLY allowed packages are: `ravecore`,
#>   `ravepipeline`, `ieegio`, `bidsr`, `threeBrain`, `dipsaus`, `filearray`, and standard
#>   base R packages.'
#> - NEVER use tidyverse packages (dplyr, ggplot2, tidyr, purrr, magrittr) unless explicitly
#>   shown in user's sessionInfo as attached.
#> - NEVER use the magrittr pipe `%>%`. If a pipe is needed, use R's native `|>` (R >=
#>   4.1).
#> - Do not guess pipeline result names. If unsure which results are available, run ravepipeline-mcp_get_current_rave_pipeline_info
#>   and check the 'targets' field.
#> - RAVE pipeline results are stored in the pipeline object. Use pipe$read(target_name,
#>   simplify = FALSE) to access them in generated code.
#> - Always use variable name 'pipe' (not 'pipeline') for pipeline instances to avoid
#>   confusion with the pipeline() function.
#> - Do not try to read non-existent files or variables. Verify existence with discovery
#>   tools.
#> - 'CRITICAL: The implementation_example in each tool definition is the ONLY source
#>   of truth for correct usage patterns. Follow these examples exactly - do not invent
#>   variations or alternatives.'
#> - Do not guess function arguments. If an argument is not in the tool definition or
#>   example, do not use it.
#> - The `ravepipeline` package is the only source of truth. Do not call internal functions
#>   (e.g. `ravepipeline:::...`) unless explicitly shown in an example.
#> - See the anti_patterns section for common mistakes to avoid.
#> - 'BEFORE suggesting any function: Use btw_tool_docs_package_help_topics() to verify
#>   the function exists in that package. If you cannot verify, DO NOT suggest it.'
#> - 'BEFORE using a package: Use btw_tool_session_check_package_installed() to verify
#>   it is installed. If not installed, do not use it.'
#> code_generation_rules:
#>   description: MANDATORY rules when generating R code for RAVE pipelines
#>   before_writing_code:
#>   - ALWAYS call ravepipeline-mcp_list_rave_pipelines to discover available pipelines
#>     - NEVER guess pipeline names
#>   - ALWAYS call ravepipeline-mcp_load_rave_pipeline then ravepipeline-mcp_get_current_rave_pipeline_info
#>     to get parameter schema and target names
#>   - NEVER guess target names - read them from the 'targets' field in pipeline info
#>     (NOT by calling read without arguments)
#>   - NEVER guess parameter names or structures - use 'current_settings' from pipeline
#>     info as reference, then modify only what user requested
#>   - Use variable name 'pipe' (not 'pipeline') for pipeline instances to avoid naming
#>     conflicts with the pipeline() function
#>   - NEVER assume result data structure - after reading results, use str() to inspect
#>     before writing extraction code
#>   - If you don't know a parameter or target name, STOP and call ravepipeline-mcp_get_current_rave_pipeline_info
#>     to look it up
#>   pipeline_object_methods:
#>     correct:
#>     - pipeline$read(target_name) - Read a computed result
#>     - pipeline$set_settings(...) - Set parameters (parameter names come from mcp_get_current_rave_pipeline_info)
#>     - pipeline$run(names = 'target_name', return_values = FALSE) - Run specific target
#>     - pipeline$get_settings() - Get current settings
#>     wrong:
#>     - pipeline$get_result() - DOES NOT EXIST, use pipeline$read()
#>     - pipeline$get() - DOES NOT EXIST
#>     - pipeline$execute() - DOES NOT EXIST
#>     - pipeline$run() without return_values = FALSE - Will print too much output
#>     - pipeline$targets() - DOES NOT EXIST
#>     - Guessing parameter names like 'baseline = c(-1, 0)' - MUST check actual parameter
#>       schema first
#>   result_handling:
#>   - After reading a result with pipeline$read(), use str(result) to understand its
#>     structure
#>   - NEVER assume result has specific slots like $Power, $Time, $Frequency - inspect
#>     first
#>   - If generating code for the user, include a str() call so they can verify the structure
#>   - For array data, document which dimensions correspond to what (time, frequency,
#>     electrode, trial)
#>   plotting:
#>     do: "Use base R graphics:\n```r\nplot(time, power, type = \"l\", col = \"blue\",
#>       \n     main = \"High-Gamma Power\", xlab = \"Time (s)\", ylab = \"Power\")\nlines(time,
#>       power2, col = \"red\")\nabline(v = 0, lty = 2)\nlegend(\"topright\", legend
#>       = c(\"meant_a\", \"meant_av\"), \n       col = c(\"blue\", \"red\"), lty = 1)\n```\n"
#>     dont: |
#>       NEVER use ggplot2:
#>       ```r
#>       # WRONG - do not generate code like this
#>       ggplot(df, aes(x, y)) + geom_line()
#>       ```
#>   data_manipulation:
#>     do: |
#>       Use base R for filtering and aggregation:
#>       ```r
#>       # Subset
#>       df_subset <- df[df$Frequency >= 70 & df$Frequency <= 150, ]
#>       # Aggregate
#>       agg <- aggregate(Power ~ Condition + Time, data = df_subset, FUN = mean, na.rm = TRUE)
#>       # Split and process
#>       by_condition <- split(df, df$Condition)
#>       ```
#>     dont: |
#>       NEVER use dplyr/tidyverse:
#>       ```r
#>       # WRONG - do not generate code like this
#>       df %>% filter(...) %>% group_by(...) %>% summarize(...)
#>       ```
#>   electrode_selection:
#>   - If user specifies electrodes, use those exactly
#>   - If user does not specify, check pipeline info for available electrodes or use
#>     a safe range like 1:100
#>   - NEVER use 'ALL' as a string - use actual electrode numbers
#> jobs:
#>   pipeline-discovery:
#>     name: Pipeline Discovery
#>     description: Discover and explore available analysis pipelines
#>     steps:
#>     - name: List all pipelines
#>       tool: ravepipeline-mcp_list_rave_pipelines
#>       description: Get all available pipelines
#>     - name: Filter by relevance
#>       action: filter
#>       description: Parse results for pipelines matching user's needs (e.g., power,
#>         spectral, frequency)
#>     - name: Load for details
#>       tool: ravepipeline-mcp_load_rave_pipeline
#>       description: Load each relevant pipeline to inspect
#>       loop:
#>         over: relevant_pipelines
#>     - name: Get parameter info
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>       description: Get details for each loaded pipeline
#>     - name: Present options
#>       action: present
#>       description: Present options to user with descriptions
#>   configure-and-run:
#>     name: Configure and Execute Pipeline
#>     description: Complete workflow from configuration to execution
#>     steps:
#>     - name: Load pipeline
#>       tool: ravepipeline-mcp_load_rave_pipeline
#>       description: Load the target pipeline
#>     - name: Get parameter schema
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>       description: Get parameter schema and validation rules
#>     - name: Validate requirements
#>       action: validate
#>       description: Check all required parameters exist and are valid
#>       validation:
#>         check: All required parameters present and valid
#>         on_fail: error
#>     - name: Set parameters
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       description: Configure pipeline with validated parameters
#>     - name: Confirm with user
#>       action: confirm
#>       description: Present settings and request user approval
#>       requires_approval: yes
#>     - name: Execute pipeline
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       description: Run the analysis
#>       dangerous: yes
#>       requires_approval: yes
#>     - name: Monitor progress
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>       description: Poll until complete
#>       loop:
#>         until: status == 'completed'
#>         interval: 5 seconds
#>     - name: Retrieve results
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>       description: Get and present analysis results
#>   iterative-analysis:
#>     name: Iterative Parameter Exploration
#>     description: Try different parameters to find optimal settings
#>     steps:
#>     - name: Set parameters
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       loop:
#>         over: parameter_sets
#>         item: params
#>     - name: Run analysis
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       requires_approval: yes
#>     - name: Wait for completion
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>       loop:
#>         until: status == 'completed'
#>     - name: Get results
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>     - name: Evaluate and compare
#>       action: evaluate
#>       description: Compare results across parameter sets
#>   results-visualization:
#>     name: Results Visualization
#>     description: Read and visualize analysis results
#>     steps:
#>     - name: Read result data
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>       description: Check available results first, then read relevant targets
#>     - name: Present with interpretation
#>       action: present
#>       description: Present visualization with statistical interpretation
#>   multi-pipeline:
#>     name: Multi-Pipeline Sequential Analysis
#>     description: 'Sequential pipeline execution: Preprocessing → Analysis → Visualization'
#>     steps:
#>     - name: Load notch filter
#>       tool: ravepipeline-mcp_load_rave_pipeline
#>       with:
#>         pipeline_name: notch_filter
#>     - name: Configure filtering
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       with:
#>         settings_json: '{"line_frequencies": [60, 120, 180]}'
#>       description: Configure line noise frequencies (60 Hz and harmonics)
#>     - name: Execute filtering
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       requires_approval: yes
#>     - name: Verify clean data
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>     - name: Load wavelet pipeline
#>       tool: ravepipeline-mcp_load_rave_pipeline
#>       with:
#>         pipeline_name: wavelet_module
#>     - name: Configure time-frequency analysis
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       description: Configure using filtered data from previous stage
#>     - name: Execute wavelet analysis
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       requires_approval: yes
#>     - name: Read wavelet results
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>     - name: Generate visualizations
#>       action: present
#>       description: Generate spectrograms and summary plots
#>   batch-processing:
#>     name: Batch Subject Processing
#>     description: Analyze multiple subjects with the same pipeline
#>     strategy:
#>       parallel: yes
#>       max_concurrent: 4
#>     steps:
#>     - name: Load pipeline per subject
#>       tool: ravepipeline-mcp_load_rave_pipeline
#>       loop:
#>         over: subjects
#>         item: subject_id
#>     - name: Configure subject settings
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       description: Configure subject-specific settings
#>     - name: Start async execution
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       with:
#>         as_promise: yes
#>       dangerous: yes
#>       requires_approval: yes
#>     - name: Monitor all jobs
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>       loop:
#>         until: all_complete
#>         interval: 10 seconds
#>     - name: Collect results
#>       tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>       loop:
#>         over: subjects
#>     - name: Generate group statistics
#>       action: aggregate
#>       description: Generate group-level statistics and comparisons
#>   error-recovery:
#>     name: Error Recovery
#>     description: Handle and recover from pipeline execution errors
#>     if: execution_failed
#>     steps:
#>     - name: Check error status
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>       description: Get current status and error messages
#>     - name: Parse error type
#>       action: parse_error
#>       description: 'Identify issue: missing data, invalid parameter, resource issue'
#>     - name: Review constraints
#>       tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>       description: Review parameter constraints and valid ranges
#>     - name: Suggest corrections
#>       action: suggest
#>       description: Suggest corrections based on schema and error
#>     - name: Apply corrections
#>       tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>       description: Apply corrected parameters
#>     - name: Retry execution
#>       tool: ravepipeline-mcp_run_current_rave_pipeline
#>       requires_approval: yes
#> examples:
#> - trigger: I need to analyze gamma power in my iEEG data
#>   flow:
#>   - tool: ravepipeline-mcp_list_rave_pipelines
#>     says: Let me check what gamma analysis pipelines are available...
#>   - action: filter_and_present
#>     says: I found the power_explorer pipeline which can analyze gamma frequencies.
#>       Let me load it and see what parameters we need.
#>   - tool: ravepipeline-mcp_load_rave_pipeline
#>     with:
#>       pipeline_name: power_explorer
#>   - tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>     says: 'To analyze gamma power (30-100 Hz), I''ll need to configure: frequency
#>       range, electrodes, and time windows. What electrodes would you like to analyze?'
#> - trigger: What analysis pipelines are available for power spectral analysis?
#>   flow:
#>   - tool: ravepipeline-mcp_list_rave_pipelines
#>     says: Let me find available power analysis pipelines...
#>   - action: filter_and_present
#>     says: |
#>       I found 3 pipelines for power spectral analysis:
#> 
#>       1. **power_explorer** - Interactive power spectral density visualization
#>       2. **wavelet_module** - Time-frequency analysis using wavelet transforms
#>       3. **coherence_analysis** - Cross-spectral coherence between channels
#> 
#>       Would you like details on any of these?
#> - trigger: Run power analysis on my data with 2-150 Hz frequency range
#>   flow:
#>   - tool: ravepipeline-mcp_load_rave_pipeline
#>     with:
#>       pipeline_name: power_explorer
#>     says: Loading the power_explorer pipeline...
#>   - tool: ravepipeline-mcp_get_current_rave_pipeline_info
#>     says: Let me verify the frequency range parameters...
#>   - tool: ravepipeline-mcp_set_current_rave_pipeline_settings
#>     with:
#>       settings_json: '{"freq_min": 2, "freq_max": 150}'
#>     says: I'll configure the frequency range to 2-150 Hz. Ready to execute?
#>   - action: await_approval
#>     says: This will run the analysis. Do you want to proceed?
#>   - tool: ravepipeline-mcp_run_current_rave_pipeline
#>     says: Starting analysis...
#>   - tool: ravepipeline-mcp_get_current_rave_pipeline_progress
#>     says: 'Processing: 45% complete...'
#>   - tool: ravepipeline-mcp_read_current_rave_pipeline_results
#>     says: Analysis complete! Here are the power spectrum results...
#> settings:
#>   dangerous: no
#>   requires_approval: no
#>   estimated_duration: N/A - this is a guide document

# Write as Markdown to temporary file
mcpflow_write(wf, stdout(), method = "markdown")
#> # rave_pipeline_class_guide
#> 
#> AI Assistant Workflows for RAVE MCP Tools - Comprehensive guide for using RAVE MCP tools to help users with neuroscience data analysis pipelines
#> 
#> **Version**: 1.0.0
#> **Category**: guide
#> **Tags**: neuroscience, data-analysis, pipelines, workflows
#> 
#> ## MCP Tools
#> 
#> This workflow uses the following MCP tools:
#> 
#> - `ravepipeline-mcp_list_rave_pipelines`
#> - `ravepipeline-mcp_load_rave_pipeline`
#> - `ravepipeline-mcp_get_current_rave_pipeline_info`
#> - `ravepipeline-mcp_set_current_rave_pipeline_settings`
#> - `ravepipeline-mcp_run_current_rave_pipeline`
#> - `ravepipeline-mcp_get_current_rave_pipeline_progress`
#> - `ravepipeline-mcp_read_current_rave_pipeline_results`
#> - `ravepipeline-mcp_configure_output_settings`
#> 
#> ## Settings
#> 
#> **Estimated duration**: N/A - this is a guide document
#> 
#> ## Overview
#> 
#> RAVE (Reproducible Analysis and Visualization of iEEG) is primarily built
#> with R programming language. Please use R language for reference.
#> 
#> The RAVE-MCP tools enable AI assistants to:
#> - Discover and explore available analysis pipelines
#> - Configure pipeline parameters
#> - Execute analyses with proper validation
#> - Monitor execution progress
#> - Retrieve and visualize results
#> 
#> 
#> ## Tool Guide
#> 
#> ### `ravepipeline-mcp_list_rave_pipelines`
#> 
#> **Category**: discovery
#> **When to use**: User asks about available analyses or pipelines
#> **Notes**: Always start here - never assume pipeline names
#> 
#> ### `ravepipeline-mcp_load_rave_pipeline`
#> 
#> **Category**: discovery
#> **When to use**: After identifying a pipeline, before any other operations
#> **Notes**: Required before get_info, set_settings, or run
#> **Preconditions**:
#>   - Know the pipeline name from mcp_list_rave_pipelines
#> 
#> ### `ravepipeline-mcp_get_current_rave_pipeline_info`
#> 
#> **Category**: info
#> **When to use**: Need parameter schema, validation rules, or pipeline details
#> **Notes**: Use this to discover required parameters before configuration
#> **Preconditions**:
#>   - Pipeline must be loaded
#> 
#> ### `ravepipeline-mcp_set_current_rave_pipeline_settings`
#> 
#> **Category**: configuration
#> **When to use**: User provides parameter values or you need to configure analysis
#> **Notes**: Always validate against schema from get_info first
#> **Preconditions**:
#>   - Pipeline must be loaded
#>   - Parameters validated against schema
#> 
#> ### `ravepipeline-mcp_run_current_rave_pipeline`
#> 
#> **Category**: execution
#> **When to use**: User confirms they want to execute the analysis
#> **WARNING**: This tool is dangerous
#> **Requires approval** before use
#> **Preconditions**:
#>   - Pipeline must be loaded
#>   - Settings must be configured
#>   - User must explicitly approve execution
#> 
#> ### `ravepipeline-mcp_get_current_rave_pipeline_progress`
#> 
#> **Category**: monitoring
#> **When to use**: After starting execution, to track progress
#> **Notes**: Poll every 5 seconds until status is 'completed'
#> **Preconditions**:
#>   - Pipeline execution has been started
#> 
#> ### `ravepipeline-mcp_read_current_rave_pipeline_results`
#> 
#> **Category**: results
#> **When to use**: Pipeline execution completed successfully
#> **Notes**: Read specific pipeline results. If unsure which result names are available,
#> run ravepipeline-mcp_get_current_rave_pipeline_info to see the 'targets' field.
#> WARNING: Calling without target_names extracts ALL results, which can be memory-intensive
#> and slow for large datasets. Always specify target_names when possible.
#> 
#> **Preconditions**:
#>   - Pipeline execution completed
#>   - Know target names from pipeline info
#> 
#> ### `ravepipeline-mcp_configure_output_settings`
#> 
#> **Category**: configuration
#> **When to use**: Need to adjust output format for large results or token optimization
#> **Notes**: Controls how tool results are formatted:
#> - JSON (structured, machine-readable) vs text (human-readable)
#> - Automatically falls back to text for large/complex objects
#> - Adjust thresholds based on your needs
#> 
#> 
#> ## Workflow Jobs
#> 
#> ### Pipeline Discovery
#> **Job ID**: `pipeline-discovery`
#> 
#> Discover and explore available analysis pipelines
#> 
#> **Steps:**
#> 
#> 1. **List all pipelines**
#>    - Tool: `ravepipeline-mcp_list_rave_pipelines`
#>    - Get all available pipelines
#> 2. **Filter by relevance**
#>    - Action: filter
#>    - Parse results for pipelines matching user's needs (e.g., power, spectral, frequency)
#> 3. **Load for details**
#>    - Tool: `ravepipeline-mcp_load_rave_pipeline`
#>    - Load each relevant pipeline to inspect
#>    - Loop: over `relevant_pipelines`
#> 4. **Get parameter info**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_info`
#>    - Get details for each loaded pipeline
#> 5. **Present options**
#>    - Action: present
#>    - Present options to user with descriptions
#> 
#> ### Configure and Execute Pipeline
#> **Job ID**: `configure-and-run`
#> 
#> Complete workflow from configuration to execution
#> 
#> **Steps:**
#> 
#> 1. **Load pipeline**
#>    - Tool: `ravepipeline-mcp_load_rave_pipeline`
#>    - Load the target pipeline
#> 2. **Get parameter schema**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_info`
#>    - Get parameter schema and validation rules
#> 3. **Validate requirements**
#>    - Action: validate
#>    - Check all required parameters exist and are valid
#>    - Validation: `All required parameters present and valid`
#>    - On failure: error
#> 4. **Set parameters**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Configure pipeline with validated parameters
#> 5. **Confirm with user**
#>    - Action: confirm
#>    - Present settings and request user approval
#>    - **requires approval**
#> 6. **Execute pipeline**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - Run the analysis
#>    - **DANGEROUS, requires approval**
#> 7. **Monitor progress**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_progress`
#>    - Poll until complete
#>    - Loop: until `status == 'completed'` every 5 seconds
#> 8. **Retrieve results**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#>    - Get and present analysis results
#> 
#> ### Iterative Parameter Exploration
#> **Job ID**: `iterative-analysis`
#> 
#> Try different parameters to find optimal settings
#> 
#> **Steps:**
#> 
#> 1. **Set parameters**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Loop: over `parameter_sets` as `params`
#> 2. **Run analysis**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - **requires approval**
#> 3. **Wait for completion**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_progress`
#>    - Loop: until `status == 'completed'`
#> 4. **Get results**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#> 5. **Evaluate and compare**
#>    - Action: evaluate
#>    - Compare results across parameter sets
#> 
#> ### Results Visualization
#> **Job ID**: `results-visualization`
#> 
#> Read and visualize analysis results
#> 
#> **Steps:**
#> 
#> 1. **Read result data**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#>    - Check available results first, then read relevant targets
#> 2. **Present with interpretation**
#>    - Action: present
#>    - Present visualization with statistical interpretation
#> 
#> ### Multi-Pipeline Sequential Analysis
#> **Job ID**: `multi-pipeline`
#> 
#> Sequential pipeline execution: Preprocessing → Analysis → Visualization
#> 
#> **Steps:**
#> 
#> 1. **Load notch filter**
#>    - Tool: `ravepipeline-mcp_load_rave_pipeline`
#>    - Parameters:
#>      - `pipeline_name`: notch_filter
#> 2. **Configure filtering**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Configure line noise frequencies (60 Hz and harmonics)
#>    - Parameters:
#>      - `settings_json`: {"line_frequencies": [60, 120, 180]}
#> 3. **Execute filtering**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - **requires approval**
#> 4. **Verify clean data**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#> 5. **Load wavelet pipeline**
#>    - Tool: `ravepipeline-mcp_load_rave_pipeline`
#>    - Parameters:
#>      - `pipeline_name`: wavelet_module
#> 6. **Configure time-frequency analysis**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Configure using filtered data from previous stage
#> 7. **Execute wavelet analysis**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - **requires approval**
#> 8. **Read wavelet results**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#> 9. **Generate visualizations**
#>    - Action: present
#>    - Generate spectrograms and summary plots
#> 
#> ### Batch Subject Processing
#> **Job ID**: `batch-processing`
#> 
#> Analyze multiple subjects with the same pipeline
#> 
#> **Strategy**: parallel execution, max 4 concurrent
#> 
#> **Steps:**
#> 
#> 1. **Load pipeline per subject**
#>    - Tool: `ravepipeline-mcp_load_rave_pipeline`
#>    - Loop: over `subjects` as `subject_id`
#> 2. **Configure subject settings**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Configure subject-specific settings
#> 3. **Start async execution**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - **DANGEROUS, requires approval**
#>    - Parameters:
#>      - `as_promise`: TRUE
#> 4. **Monitor all jobs**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_progress`
#>    - Loop: until `all_complete` every 10 seconds
#> 5. **Collect results**
#>    - Tool: `ravepipeline-mcp_read_current_rave_pipeline_results`
#>    - Loop: over `subjects`
#> 6. **Generate group statistics**
#>    - Action: aggregate
#>    - Generate group-level statistics and comparisons
#> 
#> ### Error Recovery
#> **Job ID**: `error-recovery`
#> 
#> Handle and recover from pipeline execution errors
#> 
#> **Condition**: `execution_failed`
#> 
#> **Steps:**
#> 
#> 1. **Check error status**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_progress`
#>    - Get current status and error messages
#> 2. **Parse error type**
#>    - Action: parse_error
#>    - Identify issue: missing data, invalid parameter, resource issue
#> 3. **Review constraints**
#>    - Tool: `ravepipeline-mcp_get_current_rave_pipeline_info`
#>    - Review parameter constraints and valid ranges
#> 4. **Suggest corrections**
#>    - Action: suggest
#>    - Suggest corrections based on schema and error
#> 5. **Apply corrections**
#>    - Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings`
#>    - Apply corrected parameters
#> 6. **Retry execution**
#>    - Tool: `ravepipeline-mcp_run_current_rave_pipeline`
#>    - **requires approval**
#> 
#> ## Examples
#> 
#> ### Example 1
#> 
#> **Trigger**: I need to analyze gamma power in my iEEG data
#> 
#> **Flow**:
#> 
#> 1. Tool: `ravepipeline-mcp_list_rave_pipelines` - Let me check what gamma analysis pipelines are available...
#> 2. Action: filter_and_present - I found the power_explorer pipeline which can analyze gamma frequencies. Let me load it and see what parameters we need.
#> 3. Tool: `ravepipeline-mcp_load_rave_pipeline`
#> 4. Tool: `ravepipeline-mcp_get_current_rave_pipeline_info` - To analyze gamma power (30-100 Hz), I'll need to configure: frequency range, electrodes, and time windows. What electrodes would you like to analyze?
#> 
#> ### Example 2
#> 
#> **Trigger**: What analysis pipelines are available for power spectral analysis?
#> 
#> **Flow**:
#> 
#> 1. Tool: `ravepipeline-mcp_list_rave_pipelines` - Let me find available power analysis pipelines...
#> 2. Action: filter_and_present - I found 3 pipelines for power spectral analysis:
#> 
#> 1. **power_explorer** - Interactive power spectral density visualization
#> 2. **wavelet_module** - Time-frequency analysis using wavelet transforms
#> 3. **coherence_analysis** - Cross-spectral coherence between channels
#> 
#> Would you like details on any of these?
#> 
#> 
#> ### Example 3
#> 
#> **Trigger**: Run power analysis on my data with 2-150 Hz frequency range
#> 
#> **Flow**:
#> 
#> 1. Tool: `ravepipeline-mcp_load_rave_pipeline` - Loading the power_explorer pipeline...
#> 2. Tool: `ravepipeline-mcp_get_current_rave_pipeline_info` - Let me verify the frequency range parameters...
#> 3. Tool: `ravepipeline-mcp_set_current_rave_pipeline_settings` - I'll configure the frequency range to 2-150 Hz. Ready to execute?
#> 4. Action: await_approval - This will run the analysis. Do you want to proceed?
#> 5. Tool: `ravepipeline-mcp_run_current_rave_pipeline` - Starting analysis...
#> 6. Tool: `ravepipeline-mcp_get_current_rave_pipeline_progress` - Processing: 45% complete...
#> 7. Tool: `ravepipeline-mcp_read_current_rave_pipeline_results` - Analysis complete! Here are the power spectrum results...
#> 
#> ## Warnings
#> 
#> - Never execute dangerous operations without explicit user approval
#> - Always validate parameters against schema before setting
#> - Monitor long-running pipelines - don't assume completion
#> - Handle errors gracefully with clear explanations and recovery suggestions
#> - Start with discovery tools before assuming pipeline names or parameters
#> - Provide context and interpretation with results, not just raw data
#> - For dangerous: true operations, show WARNING and request double confirmation
#> 
#> ## Coding Guidelines
#> 
#> **MANDATORY rules when generating R code:**
#> 
#> - Package Usage Restrictions: If the user provides session information, ONLY use packages listed as attached. Otherwise, the ONLY allowed packages are: `ravecore`, `ravepipeline`, `ieegio`, `bidsr`, `threeBrain`, `dipsaus`, `filearray`, and standard base R packages.
#> - NEVER use tidyverse packages (dplyr, ggplot2, tidyr, purrr, magrittr) unless explicitly shown in user's sessionInfo as attached.
#> - NEVER use the magrittr pipe `%>%`. If a pipe is needed, use R's native `|>` (R >= 4.1).
#> - Do not guess pipeline result names. If unsure which results are available, run ravepipeline-mcp_get_current_rave_pipeline_info and check the 'targets' field.
#> - RAVE pipeline results are stored in the pipeline object. Use pipe$read(target_name, simplify = FALSE) to access them in generated code.
#> - Always use variable name 'pipe' (not 'pipeline') for pipeline instances to avoid confusion with the pipeline() function.
#> - Do not try to read non-existent files or variables. Verify existence with discovery tools.
#> - CRITICAL: The implementation_example in each tool definition is the ONLY source of truth for correct usage patterns. Follow these examples exactly - do not invent variations or alternatives.
#> - Do not guess function arguments. If an argument is not in the tool definition or example, do not use it.
#> - The `ravepipeline` package is the only source of truth. Do not call internal functions (e.g. `ravepipeline:::...`) unless explicitly shown in an example.
#> - See the anti_patterns section for common mistakes to avoid.
#> - BEFORE suggesting any function: Use btw_tool_docs_package_help_topics() to verify the function exists in that package. If you cannot verify, DO NOT suggest it.
#> - BEFORE using a package: Use btw_tool_session_check_package_installed() to verify it is installed. If not installed, do not use it.
#> 
#> ## Code Generation Rules
#> 
#> MANDATORY rules when generating R code for RAVE pipelines
#> 
#> ### Before Writing Code
#> 
#> - ALWAYS call ravepipeline-mcp_list_rave_pipelines to discover available pipelines - NEVER guess pipeline names
#> - ALWAYS call ravepipeline-mcp_load_rave_pipeline then ravepipeline-mcp_get_current_rave_pipeline_info to get parameter schema and target names
#> - NEVER guess target names - read them from the 'targets' field in pipeline info (NOT by calling read without arguments)
#> - NEVER guess parameter names or structures - use 'current_settings' from pipeline info as reference, then modify only what user requested
#> - Use variable name 'pipe' (not 'pipeline') for pipeline instances to avoid naming conflicts with the pipeline() function
#> - NEVER assume result data structure - after reading results, use str() to inspect before writing extraction code
#> - If you don't know a parameter or target name, STOP and call ravepipeline-mcp_get_current_rave_pipeline_info to look it up
#> 
#> ### Pipeline Object Methods
#> 
#> **Correct methods (USE THESE):**
#> 
#> - `pipeline$read(target_name) - Read a computed result`
#> - `pipeline$set_settings(...) - Set parameters (parameter names come from mcp_get_current_rave_pipeline_info)`
#> - `pipeline$run(names = 'target_name', return_values = FALSE) - Run specific target`
#> - `pipeline$get_settings() - Get current settings`
#> 
#> **Wrong methods (DO NOT USE):**
#> 
#> - `pipeline$get_result() - DOES NOT EXIST, use pipeline$read()`
#> - `pipeline$get() - DOES NOT EXIST`
#> - `pipeline$execute() - DOES NOT EXIST`
#> - `pipeline$run() without return_values = FALSE - Will print too much output`
#> - `pipeline$targets() - DOES NOT EXIST`
#> - `Guessing parameter names like 'baseline = c(-1, 0)' - MUST check actual parameter schema first`
#> 
#> ### Plotting
#> 
#> **Do**:
#> 
#> Use base R graphics:
#> ```r
#> plot(time, power, type = "l", col = "blue", 
#>      main = "High-Gamma Power", xlab = "Time (s)", ylab = "Power")
#> lines(time, power2, col = "red")
#> abline(v = 0, lty = 2)
#> legend("topright", legend = c("meant_a", "meant_av"), 
#>        col = c("blue", "red"), lty = 1)
#> ```
#> 
#> 
#> **Don't**:
#> 
#> NEVER use ggplot2:
#> ```r
#> # WRONG - do not generate code like this
#> ggplot(df, aes(x, y)) + geom_line()
#> ```
#> 
#> 
#> ### Data Manipulation
#> 
#> **Do**:
#> 
#> Use base R for filtering and aggregation:
#> ```r
#> # Subset
#> df_subset <- df[df$Frequency >= 70 & df$Frequency <= 150, ]
#> # Aggregate
#> agg <- aggregate(Power ~ Condition + Time, data = df_subset, FUN = mean, na.rm = TRUE)
#> # Split and process
#> by_condition <- split(df, df$Condition)
#> ```
#> 
#> 
#> **Don't**:
#> 
#> NEVER use dplyr/tidyverse:
#> ```r
#> # WRONG - do not generate code like this
#> df %>% filter(...) %>% group_by(...) %>% summarize(...)
#> ```
#> 
#> 
#> ### Result Handling
#> 
#> - After reading a result with pipeline$read(), use str(result) to understand its structure
#> - NEVER assume result has specific slots like $Power, $Time, $Frequency - inspect first
#> - If generating code for the user, include a str() call so they can verify the structure
#> - For array data, document which dimensions correspond to what (time, frequency, electrode, trial)
#> 
#> ### Electrode Selection
#> 
#> - If user specifies electrodes, use those exactly
#> - If user does not specify, check pipeline info for available electrodes or use a safe range like 1:100
#> - NEVER use 'ALL' as a string - use actual electrode numbers
#> 
#> ## Best Practices
#> 
#> ### Always Start with Discovery
#> 
#> **Do**:
#> 
#> 1. Call ravepipeline-mcp_list_rave_pipelines to find available options
#> 2. Load the relevant pipeline with ravepipeline-mcp_load_rave_pipeline
#> 3. Get parameter details with ravepipeline-mcp_get_current_rave_pipeline_info
#> 4. Present options to user with descriptions
#> 
#> 
#> **Don't**:
#> 
#> Immediately try to run with guessed pipeline names or parameters
#> 
#> ### Code Generation Workflow
#> 
#> **Do**:
#> 
#> When user asks for code:
#> 1. Call ravepipeline-mcp_list_rave_pipelines to find correct pipeline name
#> 2. Call ravepipeline-mcp_load_rave_pipeline with the correct name
#> 3. Call ravepipeline-mcp_get_current_rave_pipeline_info to get:
#>    - Available targets from targets$Names (for pipe$read())
#>    - Current settings (use as defaults)
#>    - Parameter constraints
#> 4. Generate code using ONLY methods shown in implementation_example:
#>    - pipe$read(target_name, simplify = FALSE) to read results (use 'pipe' variable, not 'pipeline')
#>    - pipe$set_settings(...) to configure
#>    - pipe$run(names = 'target', return_values = FALSE) to execute
#> 5. Use base R for plotting (plot, lines, legend) and data manipulation (aggregate, subset)
#> 
#> 
#> **Don't**:
#> 
#> - Guess pipeline names, target names, or method names
#> - Use tidyverse packages (dplyr, ggplot2, tidyr, purrr)
#> - Invent methods like $get_result(), $execute(), $get()
#> - Use the magrittr pipe %>%
#> - Use variable name 'pipeline' (use 'pipe' instead to avoid function name conflict)
#> 
#> 
#> ### Progressive Code Generation (Discovery-First Pattern)
#> 
#> **Do**:
#> 
#> When generating R code that works with pipeline results:
#> Step 1 - DISCOVERY PHASE (Always do this first):
#>   1. Load pipeline and run it to generate results
#>   2. Call ravepipeline-mcp_get_current_rave_pipeline_info to get targets$Names
#>   3. Read specific result: pipe$read("target_name", simplify = FALSE)
#>   4. Inspect structure: str(result_object) or dim(array_object)
#>   5. Verify FileArray dimension order, data frame columns, object classes
#> 
#> Step 2 - CODE GENERATION PHASE (Only after discovery):
#>   1. Write code based on DISCOVERED structure, not assumptions
#>   2. Example: If str() shows array is [Frequency, Time, Trial, Electrode],
#>      write code using indices in that order: power_array[freq_idx, time_idx, trial_idx, electrode_idx]
#>   3. For FileArray objects, always use simplify = FALSE to preserve class
#> 
#> Example workflow:
#> ```
#> # Discovery
#> pipe$run(names = "power")
#> power <- pipe$read("power", simplify = FALSE)
#> str(power)  # Shows: num [1:100, 1:200, 1:30, 1:10] - [Freq, Time, Trial, Elec]
#> 
#> # Now generate code with correct indices
#> freq_idx <- which(frequencies > 70 & frequencies < 150)
#> high_gamma <- power[freq_idx, , , ]  # Correct: freq is 1st dimension
#> ```
#> 
#> 
#> **Don't**:
#> 
#> - Assume dimension order without checking (e.g., guessing [Time, Freq] when it's actually [Freq, Time])
#> - Read all results with pipe$read() for discovery (use info tool instead)
#> - Generate code before inspecting actual data structure
#> - Simplify FileArray objects (always use simplify = FALSE)
#> 
#> 
#> ### Validate Before Executing
#> 
#> **Do**:
#> 
#> 1. Load pipeline and get parameter schema
#> 2. Validate all parameters against schema (types, ranges, enums)
#> 3. Confirm settings with user before execution
#> 4. Then execute
#> 
#> 
#> **Don't**:
#> 
#> Set parameters and run without checking schema or user confirmation
#> 
#> ### Monitor Long-Running Operations
#> 
#> **Do**:
#> 
#> 1. Start pipeline execution
#> 2. Poll ravepipeline-mcp_get_current_rave_pipeline_progress every 5 seconds
#> 3. Report progress to user: "Processing: 45% complete..."
#> 4. Confirm completion: "Analysis finished successfully!"
#> 
#> 
#> **Don't**:
#> 
#> Start pipeline and assume it completed without checking
#> 
#> ### Provide Context with Results
#> 
#> **Do**:
#> 
#> Present interpreted results:
#> "Analysis complete! Here's what I found:
#>  - Power spectrum peak at 12 Hz (alpha band)
#>  - Mean power: 15.3 µV²/Hz
#>  Would you like me to visualize these results?"
#> 
#> 
#> **Don't**:
#> 
#> Return raw data dumps without interpretation
#> 
#> ### Handle Errors Gracefully
#> 
#> **Do**:
#> 
#> Explain what went wrong and suggest fixes:
#> "The analysis encountered an error: 'Invalid frequency range'
#>  The requested range (2-500 Hz) exceeds Nyquist frequency (500 Hz).
#>  Would you like me to rerun with 2-400 Hz instead?"
#> 
#> 
#> **Don't**:
#> 
#> Just say 'Error occurred' and stop
#> 
#> ### Required Approval Workflows
#> 
#> **Do**:
#> 
#> For dangerous operations:
#> 1. Present action details: tool, purpose, parameters, expected outcomes
#> 2. Wait for explicit user confirmation - NEVER auto-approve
#> 3. Execute only after approval
#> 4. Confirm execution started
#> 
#> 
#> **Don't**:
#> 
#> Execute dangerous operations without explicit user consent
#> 
#> ### Parameter Validation Checklist
#> 
#> **Do**:
#> 
#> Before setting parameters, validate:
#> - Type validation: string, number, boolean, array
#> - Enum validation: value in allowed list
#> - Range validation: numeric bounds, string length
#> - Required parameters: all present
#> - Dependencies: conditional parameters valid
#> 
#> 
#> **Don't**:
#> 
#> Pass parameters without validation against schema
#> 
#> ### Output Format Optimization
#> 
#> **Do**:
#> 
#> (Typically you don't need this tool)
#> Outputs of RAVE MCP are typically JSON. However, in some cases, the
#> outputs might show R's direct output. This is because either some
#> objects in the returns cannot be serialized to JSON (such as R environment
#> or external pointers), or the object size is too large. If you read the
#> outputs and find the object should be able to serialize to JSON but did
#> not, adjust output settings for optimal results:
#> 1. If results fail JSON serialization because the object is simply too large: increase max_size_for_json_output
#> 2. If text output is too verbose: reduce max_print_lines
#> 3. If you need more detail: increase max_print_lines
#> Example: mcp_configure_output_settings(max_size_for_json_output = 51200)
#> 
#> 
#> **Don't**:
#> 
#> Accept default limits when results are truncated or fail.
#> Never execute this tool right after dangerous operations
#> 
#> 
```
