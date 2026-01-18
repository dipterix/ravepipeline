# AI Assistant Workflows for RAVE MCP Tools

This guide provides recommended workflows for AI assistants using RAVE MCP tools to help users with neuroscience data analysis pipelines.

## Overview

RAVE (R Analysis and Visualization of iEEG) MCP tools enable AI assistants to:
- Discover and explore available analysis pipelines
- Configure pipeline parameters
- Execute analyses with proper validation
- Monitor execution progress
- Retrieve and visualize results

## Available Tools

### Discovery & Information
- `mcp_list_rave_pipelines`: List all available pipelines
- `mcp_get_rave_pipeline_info`: Get detailed pipeline information

### Pipeline Management
- `mcp_load_rave_pipeline`: Load and initialize a pipeline
- `mcp_set_rave_pipeline_settings`: Configure pipeline parameters

### Execution & Monitoring
- `mcp_run_rave_pipeline`: Execute a pipeline analysis
- `mcp_get_rave_pipeline_progress`: Monitor execution status

### Results
- `mcp_read_rave_pipeline_results`: Read analysis outputs
- `mcp_visualize_rave_pipeline`: Generate visualizations

## Basic Workflow Patterns

### Pattern 1: Pipeline Discovery

**User Request**: "What analysis pipelines are available for power spectral analysis?"

**Workflow**:
```
1. mcp_list_rave_pipelines()
   -> Get all available pipelines

2. Parse results for relevant pipelines
   -> Filter by keywords: "power", "spectral", "frequency"

3. mcp_get_rave_pipeline_info(pipeline_name)
   -> Get details for each relevant pipeline

4. Present options to user with descriptions
```

**Example**:
```
Assistant: I found 3 pipelines for power spectral analysis:

1. **power_explorer** - Interactive power spectral density visualization
2. **wavelet_module** - Time-frequency analysis using wavelet transforms
3. **coherence_analysis** - Cross-spectral coherence between channels

Would you like details on any of these?
```

### Pattern 2: Configure and Run

**User Request**: "Run power analysis on my data with 2-150 Hz frequency range"

**Workflow**:
```
1. mcp_get_rave_pipeline_info("power_explorer")
   -> Get parameter schema

2. Validate user requirements against schema
   -> Check frequency range parameter exists
   -> Verify type and valid ranges

3. mcp_load_rave_pipeline("power_explorer")
   -> Initialize pipeline

4. mcp_set_rave_pipeline_settings(
     pipeline_name = "power_explorer",
     settings_json = '{"freq_min": 2, "freq_max": 150, ...}'
   )
   -> Configure parameters

5. Confirm settings with user (if requires_approval)

6. mcp_run_rave_pipeline("power_explorer")
   -> Execute analysis

7. mcp_get_rave_pipeline_progress("power_explorer")
   -> Monitor until complete

8. mcp_read_rave_pipeline_results("power_explorer")
   -> Retrieve results
```

### Pattern 3: Iterative Analysis

**User Request**: "Try different frequency ranges to find optimal parameters"

**Workflow**:
```
For each parameter set:
  1. mcp_set_rave_pipeline_settings(...)
  2. mcp_run_rave_pipeline(...)
  3. mcp_get_rave_pipeline_progress(...) [wait for completion]
  4. mcp_read_rave_pipeline_results(...)
  5. Evaluate results
  6. Adjust parameters based on results

Present comparison of all runs
```

### Pattern 4: Results Visualization

**User Request**: "Show me the power spectrum results"

**Workflow**:
```
1. mcp_read_rave_pipeline_results(
     pipeline_name = "power_explorer",
     target_names = ["plot_data", "statistics"]
   )
   -> Get result data

2. mcp_visualize_rave_pipeline(
     pipeline_name = "power_explorer",
     plot_type = "power_spectrum"
   )
   -> Generate visualization

3. Present visualization with interpretation
```

## Advanced Workflows

### Multi-Pipeline Workflow

**Scenario**: Preprocessing -> Analysis -> Visualization

```
1. Run notch filter pipeline
   - Configure line noise frequencies (60 Hz, harmonics)
   - Execute filtering
   - Verify clean data

2. Run wavelet analysis pipeline
   - Use filtered data from step 1
   - Configure time-frequency parameters
   - Execute wavelet transform

3. Run visualization pipeline
   - Load wavelet results
   - Generate spectrograms
   - Create summary plots

4. Present integrated results
```

### Batch Processing Workflow

**Scenario**: Analyze multiple subjects/sessions

```
For each subject:
  1. mcp_load_rave_pipeline("batch_processor")
  2. Configure subject-specific settings
  3. mcp_run_rave_pipeline(as_promise = TRUE)
     -> Start async execution

Monitor all jobs:
  - Periodically check progress for all subjects
  - Report completion status
  
Aggregate results:
  - Collect results from all subjects
  - Generate group-level statistics
  - Create comparison visualizations
```

### Error Recovery Workflow

**Scenario**: Pipeline execution fails

```
1. mcp_get_rave_pipeline_progress(pipeline_name)
   -> Check error status

2. Parse error message
   -> Identify issue (missing data, invalid parameter, etc.)

3. If parameter issue:
   - mcp_get_rave_pipeline_info(pipeline_name)
   - Review parameter constraints
   - Suggest corrections to user

4. If data issue:
   - Guide user through data validation
   - Suggest preprocessing steps

5. Retry with corrections:
   - mcp_set_rave_pipeline_settings(...)
   - mcp_run_rave_pipeline(...)
```

## Safety and Validation

### Required Approval Workflows

For tools with `requires_approval: true`:

```
1. Present action details to user
   - Tool name and purpose
   - All parameters and values
   - Expected outcomes
   - Potential side effects

2. Wait for explicit user confirmation
   - "Type 'approve' to proceed"
   - Do NOT auto-approve

3. Execute only after approval

4. Confirm execution started
```

### Dangerous Operations

For tools with `dangerous: true`:

```
1. Extra confirmation required
   - Show WARNING clearly
   - Explain risks
   - Suggest safer alternatives if available

2. Double confirmation
   - "Are you sure? This action cannot be undone."
   
3. Log the operation
   - Record what was approved
   - Timestamp and user confirmation

4. Verify results
   - Check for expected outcomes
   - Alert user to any anomalies
```

### Parameter Validation

Before setting parameters:

```
1. Check against schema
   - Type validation (string, number, boolean, etc.)
   - Enum validation (if specified)
   - Array type validation

2. Range validation
   - Numeric bounds (if documented)
   - String length limits
   - Array length constraints

3. Dependency validation
   - Required parameters present
   - Conditional parameters valid
   - Parameter relationships satisfied

4. Warn user of issues
   - Clear error messages
   - Suggest corrections
   - Provide examples
```

## Best Practices

### 1. Always Start with Discovery

```
[BAD]:
User: "Run power analysis"
Assistant: [Immediately tries to run with guessed parameters]

[GOOD]:
User: "Run power analysis"
Assistant: [Calls mcp_list_rave_pipelines()]
"I found several power analysis pipelines. Let me get details..."
[Calls mcp_get_rave_pipeline_info(...)]
"The power_explorer pipeline needs these parameters: ..."
```

### 2. Validate Before Executing

```
[BAD]:
Assistant: [Sets parameters and runs without checking]

[GOOD]:
Assistant: [Gets pipeline info first]
[Validates all parameters against schema]
[Confirms settings with user]
[Then executes]
```

### 3. Monitor Long-Running Operations

```
[BAD]:
Assistant: [Starts pipeline, assumes it completed]

[GOOD]:
Assistant: [Starts pipeline]
"Pipeline started. Checking progress..."
[Polls mcp_get_rave_pipeline_progress every 5 seconds]
"Processing: 45% complete..."
"Analysis finished successfully!"
```

### 4. Provide Context with Results

```
[BAD]:
Assistant: [Returns raw data dump]

[GOOD]:
Assistant: "Analysis complete! Here's what I found:

Power spectrum peak at 12 Hz (alpha band)
Mean power: 15.3 uV^2/Hz
95th percentile: 28.1 uV^2/Hz

Would you like me to visualize these results?"
```

### 5. Handle Errors Gracefully

```
[BAD]:
Assistant: "Error occurred." [Stops]

[GOOD]:
Assistant: "The analysis encountered an error: 'Invalid frequency range'

This means the requested frequency range (2-500 Hz) exceeds the 
data's sampling rate (1000 Hz). The maximum valid frequency is 
500 Hz (Nyquist frequency).

Would you like me to rerun with 2-400 Hz instead?"
```

## Example Conversations

### Complete Discovery-to-Results Workflow

```
User: I need to analyze gamma power in my iEEG data