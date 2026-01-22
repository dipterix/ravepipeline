
setwd("/Users/dipterix/Dropbox (Personal)/projects/ravepipeline")

devtools::load_all()

# NOTE: reprex runs in a SEPARATE R process that uses the INSTALLED package,
# not the dev version. Run directly or use devtools::install() first.

# Load the workflow guide
chat <- ellmer::chat_anthropic()
wf <- ravepipeline::mcpflow_read("ravepipeline::rave_pipeline_class_guide")

# Create the chat with RAVE MCP tools
chat <- ravepipeline::mcpflow_instantiate(wf, chat = chat)
names(chat$get_tools())

# Register btw tools for package discovery (docs + session groups)
# This allows AI to verify functions exist before suggesting them
toolset1 <- lapply(ravepipeline::mcptool_load_all("ravepipeline", groups = c("search", "docs")), function(def) {
  ravepipeline::mcptool_instantiate(def, state_env = attr(chat, "rave_state_env"))
})
names(toolset1)

chat$register_tools(toolset1)


# ============================================================
# CAPTURE TOOL CALLS (the "thinking" process)
# ============================================================
tool_log <- list()

# Log tool requests (when AI decides to call a tool)
chat$on_tool_request(function(request) {
  entry <- list(
    type = "request",
    time = Sys.time(),
    tool = request@name,
    arguments = request@arguments
  )
  tool_log <<- c(tool_log, list(entry))
  cat("\n[TOOL REQUEST]", request@name, "\n")
  cat("  Arguments:", jsonlite::toJSON(request@arguments, auto_unbox = TRUE), "\n")
})

# Log tool results (what the tool returned)
chat$on_tool_result(function(result) {
  # ContentToolResult has different slots - use what's available
  result_str <- tryCatch(
    as.character(result@value),
    error = function(e) tryCatch(
      as.character(result),
      error = function(e2) "(unable to capture result)"
    )
  )
  entry <- list(
    type = "result",
    time = Sys.time(),
    value = substr(result_str, 1, 500)  # Truncate for readability
  )
  tool_log <<- c(tool_log, list(entry))
  cat("[TOOL RESULT]", substr(result_str, 1, 200), "...\n")
})

# Print system prompt to verify new sections are included
system_prompt <- chat$get_system_prompt()
cat("=== System Prompt Preview (last 200 lines) ===\n")
prompt_lines <- strsplit(system_prompt, "\n")[[1]]
cat(tail(prompt_lines, 200), sep = "\n")

# ============================================================
# CHAT WITH ECHO ENABLED
# ============================================================
# echo = "all" streams input AND output to console
# echo = "output" streams only AI responses
response <- chat$chat(
  btw::btw(
    sessionInfo(),
    "I'm a tester to your ability to do a task. Please ignore your cache as if you were answering this question for the first time. Here is the question: I want to know the project `demo` subject `DemoSubject` what's the high-gamma (70-150 Hz) power data change compared to baseline (-1~0 s) and plot the collapsed power over time for meant_a vs meant_av for electrode 14 only. Show me the code"
  ),
  echo = "all"  # Stream everything to console
)

# ============================================================
# SAVE OUTPUT WITH FULL TOOL LOG
# ============================================================

# # Format tool log for output
# tool_log_text <- sapply(tool_log, function(entry) {
#   if (entry$type == "request") {
#     sprintf("[%s] TOOL REQUEST: %s\n  Args: %s",
#             format(entry$time, "%H:%M:%S"),
#             entry$tool,
#             jsonlite::toJSON(entry$arguments, auto_unbox = TRUE))
#   } else {
#     sprintf("[%s] TOOL RESULT: %s...",
#             format(entry$time, "%H:%M:%S"),
#             substr(entry$value, 1, 200))
#   }
# })

cat("\n\n==== RESPONSES ======\n\n")
cat(response, sep = "\n")

# cd '/Users/dipterix/Dropbox (Personal)/projects/ravepipeline' && Rscript 'adhoc/AI_troubleshoot.r' > 'adhoc/AI_troubleshoot.md' 2>&1

toolset2 <- lapply(ravepipeline::mcptool_load_all("ravepipeline", groups = c("run")), function(def) {
  ravepipeline::mcptool_instantiate(def, state_env = attr(chat, "rave_state_env"))
})

chat$register_tools(toolset2)
response2 <- chat$chat("Now you have code execution right. please run the code for me and show me the correct code. also highlight critical changes")

chat$chat(r"(I'm revealing the actual answer.
``` r
# Load the power_explorer pipeline
library(ravepipeline)
pipe <- pipeline("power_explorer")

# Configure settings for demo/DemoSubject, electrode 14
# Baseline: -1 to 0 seconds
pipe$set_settings(
  project_name = "demo",
  subject_code = "DemoSubject",
  loaded_electrodes = "14",
  analysis_electrodes = "14",
  baseline_settings = list(
    window = list(c(-1.0, 0.0)),
    scope = "Per frequency, trial, and electrode",
    unit_of_analysis = "% Change Power"
  ),
  first_condition_groupings = list(
    `1` = list(
      label = "meant_a",
      conditions = c("meant_a")
    ),
    `2` = list(
      label = "meant_av",
      conditions = c("meant_av")
    )
  )
)

# Run the pipeline to compute baselined power
pipe$run(names = "over_time_by_condition_data", return_values = FALSE)

# Read the computed data
result <- pipe$read("over_time_by_condition_data", simplify = FALSE)
runtime_env <- pipe$shared_env()

runtime_env$plot_over_time_by_condition(result$over_time_by_condition_data)

```

Although your second code worked. This does not mean your code is perfect. There are multiple ways to make the code work. My goal is to improve the MCP tools so you can generate code closer to what I expected.

Now let's reflect. I noticed the first answer differs from the second answer. What caused this difference? Also let me know is there is any confusing prompts/results/context that you got during the conversation. What tools you should have used but not realized you could? If you were me, how would you recommend me to do so that you can better understand this workflow?)")


# ===== AI reflection ======
#
# Excellent question! Let me reflect on what happened and provide concrete recommendations.
#
# ## What Caused the Difference Between First and Second Answers?
#
# ### Key Differences:
#
# 1. **I missed the helper functions entirely** - I didn't call `ravepipeline-mcp_tool_pipeline_get_helpers` to discover
# `plot_over_time_by_condition()`
# 2. **I missed the pre-computed target** - I should have noticed `over_time_by_condition_data` in the targets list and used it
# instead of manually processing
# 3. **I didn't use `first_condition_groupings`** - I manually filtered trials instead of using the pipeline's built-in grouping
# mechanism
# 4. **I missed `loaded_electrodes`** - I only set `analysis_electrodes` but not `loaded_electrodes`, which could cause issues
#
# ### Root Causes:
#
# 1. **I jumped to execution too quickly** without fully exploring what the pipeline offered
# 2. **I didn't follow my own workflow guidelines** - The guide says "DO: Call this before writing custom utility code" for
# `get_helpers`, but I ignored it
# 3. **I didn't carefully examine the targets list** - I saw `over_time_by_condition_data` but didn't think "this is exactly what the
# user wants!"
#
# ## Confusing Prompts/Results/Context
#
# ### 2. **Helper Functions Are Hidden**
# The workflow says to call `...get_helpers`, but:
# - It's easy to forget when you're focused on "just run the analysis"
# - The tool returns 119 function signatures - that's overwhelming
# - I don't know which helpers are relevant without reading all of them
#
# **Recommendation**: When `get_info` returns targets, also show a hint like:
# ```
# Tip: This pipeline has 119 helper functions. Call get_helpers to discover
# plotting and analysis utilities before writing custom code.
# ```
#
# ### 3. **`current_settings` Showed Pre-configured Values**
# When I called `...get_info` tool, I saw:
# ```r
# $current_settings$first_condition_groupings
# $current_settings$first_condition_groupings$`1`$label
# [1] "audio_visual"
# $current_settings$first_condition_groupings$`1`$conditions
# [1] "known_av" "meant_av" "last_av"  "drive_av"
# ```
#
# This confused me because:
# - I saw `meant_av` was already in a group
# - But the user wanted **only** `meant_a` vs `meant_av` (not all audio_visual conditions)
# - I didn't realize I should **replace** this entirely
#
# **This was actually helpful**, but I misinterpreted it as "this is how groupings work" rather than "these are the current settings
# I need to override."
#
# ## Tools I Should Have Used But Didn't
#
# ### 1. ❌ `ravepipeline-mcp_tool_pipeline_get_helpers` (CRITICAL MISS)
# **Why I should have used it:**
#   - Would have discovered `plot_over_time_by_condition()` immediately
# - Would have seen other utilities for data manipulation
# - Would have avoided reinventing the wheel
#
# **Why I didn't:**
# - I was in "execution mode" after seeing the pipeline info
# - The workflow guide mentions it, but it didn't feel urgent
#
# **Recommendation**: Make this tool feel more "mandatory" in the workflow. Perhaps:
#   ```
# After loading a pipeline and before writing custom code:
#   1. MUST call get_info to see targets and parameters
# 2. MUST call get_helpers to see available utilities
# 3. THEN decide whether to use existing targets/helpers or write custom code
# ```
#
# ### 2. ⚠️ `ravepipeline-mcp_tool_pipeline_get_script` (Partial Use)
# **I should have used this with a pattern** like:
#   ```r
# pattern = "export.*some_potential_target"
# ```
# To search for how `over_time_by_condition_data` is computed and what settings it uses.
#
# **Why I didn't:**
# - I was worried about token usage (the warning scared me)
# - I thought I could figure it out from `get_info` alone
# - I didn't know what pattern to search for
#
# ### 3. ⚠️ Better use of `...get_info` targets
# I glanced at the targets list but didn't **think critically** about what each target does. I should have asked myself:
# - "What does `over_time_by_condition_data` produce?"
# - "Does this target already do what the user wants?"
# - "What settings does this target depend on?"
#
# ## Recommendations to Improve the Workflow
#
# ### For the MCP Tools:
#
# #### 1. **Add a "Suggest Target" Tool**
# ```
# ravepipeline-mcp_tool_suggest_targets(user_query)
# ```
# Input: "I want to plot high-gamma power over time comparing two conditions"
# Output:
# ```
# Suggested targets:
# 1. over_time_by_condition_data (BEST MATCH)
#    - Computes power over time grouped by condition
#    - Requires: first_condition_groupings setting
#    - Output: Data frame with Time, Condition, Power columns
#
# 2. pluriform_power (alternative)
#    - Raw power array, requires manual processing
#
# Suggested helpers:
# 1. plot_over_time_by_condition() - Plots condition comparison
# 2. collapse_power() - Averages across frequencies
# ```
#
# This would guide me to the right solution immediately.
#
# #### 2. **Enhance `get_info` to Show Target Relationships**
# ```
# $targets
#   Name: over_time_by_condition_data
#   Description: Build over time by condition data
#   Dependencies: baselined_power, analysis_groups, first_condition_groupings
#   Output type: data.frame with columns [Time, Condition, Power, SE]
#   Common use: Plotting power time courses by condition
#   Related helpers: plot_over_time_by_condition()
# ```
#
# This would help me understand what each target produces and when to use it.
#
# #### 3. **Make `get_helpers` Smarter**
# Instead of returning 119 functions, allow filtering:
# ```
# ravepipeline-mcp_tool_pipeline_get_helpers(category = "plotting")
# # Returns only plot_* functions
#
# ravepipeline-mcp_tool_pipeline_get_helpers(pattern = "condition")
# # Returns functions with "condition" in name
# ```
#
# Or provide categories:
# ```
# Helper function categories:
# - Plotting (23 functions): plot_*, draw_*, render_*
# - Data manipulation (45 functions): collapse_*, aggregate_*, filter_*
# - Statistical analysis (18 functions): test_*, compare_*, baseline_*
# - Utilities (33 functions): format_*, validate_*, convert_*
# ```
#
# #### 4. **Add Examples to Tool Descriptions**
# For `mcp_tool_pipeline_set_settings`, show common patterns:
# ```
# Common settings patterns:
#
# # Pattern 1: Analyze specific conditions
# first_condition_groupings = list(
#   `1` = list(label = "condition_a", conditions = c("condition_a")),
#   `2` = list(label = "condition_b", conditions = c("condition_b"))
# )
#
# # Pattern 2: Custom baseline window
# baseline_settings = list(
#   window = list(c(-1.0, 0.0)),
#   scope = "Per frequency, trial, and electrode",
#   unit_of_analysis = "% Change Power"
# )
# ```
#
# ### For the Workflow Guide:
#
# #### 5. **Add a "Decision Tree" Section**
# ```
# User wants to: Compare conditions over time
# ├─ Does pipeline have "over_time_by_condition" target? → YES
# │  └─ Use it! Set first_condition_groupings, run target, use helper plot function
# └─ NO
#    └─ Read baselined_power, manually process, plot with base R
# ```
#
# #### 6. **Add "Anti-Patterns" Examples**
# ```
# ❌ DON'T: Manually filter trials and compute averages when pipeline has built-in targets
# ✅ DO: Check targets list for "by_condition", "by_trial", "collapsed" etc.
#
# ❌ DON'T: Write custom plotting code before checking helpers
# ✅ DO: Call get_helpers and search for plot_* functions
# ```
#
# #### 7. **Make the Workflow More Explicit**
# Current workflow is:
# ```
# 1. List pipelines
# 2. Load pipeline
# 3. Get info
# 4. Set settings
# 5. Run
# 6. Read results
# ```
#
# Better workflow:
# ```
# 1. List pipelines → Find the right pipeline
# 2. Load pipeline → Make it available
# 3. Get info → See targets and parameters
# 4. Get helpers → See available utilities ⭐ NEW
# 5. Identify target → Match user request to existing target ⭐ NEW
# 6. Set settings → Configure for your use case
# 7. Run target → Execute specific target (not whole pipeline)
# 8. Read results → Get computed data
# 9. Use helpers → Plot/analyze with helper functions ⭐ NEW
# ```
#
# ### For Your Specific Case:
#
# #### 8. **Better Parameter Descriptions in YAML**
# In the pipeline's `settings.yaml`, add examples:
#   ```yaml
# first_condition_groupings:
#   type: list
# description: Group trials by condition for comparison
# example:
#   - label: "auditory_only"
# conditions: ["meant_a", "known_a"]
# - label: "audio_visual"
# conditions: ["meant_av", "known_av"]
# use_case: "Required for over_time_by_condition_data target"
# ```
#
# Then `get_info` could show these examples.
#
# #### 9. **Add a "Quick Start" for Common Tasks**
# ```
# ravepipeline-mcp_tool_pipeline_quick_start(task = "compare_conditions")
# ```
# Output:
#   ```
# To compare two conditions:
#   1. Set first_condition_groupings with your conditions
# 2. Run target: over_time_by_condition_data
# 3. Plot with: helpers$plot_over_time_by_condition(result)
#
# Example code:
#   pipe$set_settings(
#     first_condition_groupings = list(
#       `1` = list(label = "cond_a", conditions = c("cond_a")),
#       `2` = list(label = "cond_b", conditions = c("cond_b"))
#     )
#   )
# pipe$run(names = "over_time_by_condition_data")
# result <- pipe$read("over_time_by_condition_data")
# helpers <- pipe$shared_env()
# helpers$plot_over_time_by_condition(result)
# ```
#
# ## Summary
#
# **The core issue**: I treated this as a "data processing" task instead of a "pipeline usage" task.
#
# **I should have**:
#   1. Called `get_helpers` to discover plotting functions
# 2. Carefully examined targets to find `over_time_by_condition_data`
# 3. Used `first_condition_groupings` instead of manual filtering
# 4. Trusted the pipeline's built-in functionality
#
# **To fix this**, you should:
# 1. Make `get_helpers` feel mandatory (not optional)
# 2. Improve target descriptions to show what they output and when to use them
# 3. Add a "suggest target" tool that maps user requests to pipeline targets
# 4. Show relationships between targets, settings, and helpers in `get_info`
# 5. Provide common usage patterns and examples directly in tool outputs
#
# The goal should be: **AI should discover the "right way" through tools, not guess-and-check.**
