
setwd("/Users/dipterix/Dropbox (Personal)/projects/ravepipeline")

devtools::load_all()

# NOTE: reprex runs in a SEPARATE R process that uses the INSTALLED package,
# not the dev version. Run directly or use devtools::install() first.

# Load the workflow guide
chat <- ellmer::chat_anthropic()
wf <- ravepipeline::mcpflow_read("ravepipeline::rave_pipeline_class_guide")

# Create the chat with RAVE MCP tools
chat <- ravepipeline::mcpflow_instantiate(wf, chat = chat)

# Register btw tools for package discovery (docs + session groups)
# This allows AI to verify functions exist before suggesting them
chat$register_tools(btw::btw_tools("docs", "session", "search"))


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


chat$register_tools(btw::btw_tools("env", "run", "files"))
response2 <- chat$chat("Now you have code execution right. please run the code for me and show me the correct code")

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

Although your second code worked. This does not mean your code is inferior. There are multiple ways to make the code work. My goal is to improve the MCP tools so you can generate code closer to what I expected.

Now let's reflect. I noticed the first answer you gave to me had some inconsistency with the second answer. What caused this difference? If you were me, how would you recommend me to do so that you can better understand this workflow? Also let me know is there is any confusing prompts/results/context that you got during the conversation.)")

