
# ravepipeline

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dipterix/ravepipeline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/ravepipeline/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/ravepipeline)](https://CRAN.R-project.org/package=ravepipeline)
[![ravepipeline status badge](https://rave-ieeg.r-universe.dev/ravepipeline/badges/version)](https://rave-ieeg.r-universe.dev/ravepipeline)
<!-- badges: end -->

The goal of ravepipeline is to provide pipeline infrastructure for [Reproducible Analysis and Visualization of Intracranial Electroencephalography (`RAVE`)](https://rave.wiki). The package defines high-level class to build, compile, set, execute, and share neuroscience pipelines. Both `R` and `Python` are supported with `Markdown` and `shiny` dashboard templates for extending and building customized pipelines.

We offers several built-in pipelines, see repository [rave-ieeg/rave-pipelines](https://github.com/rave-ieeg/rave-pipelines) for details.

More documentation is available at [rave.wiki](https://rave.wiki)

## Installation

Please check out our [full installation guide on how to install 'RAVE'](https://rave.wiki/posts/installation/installation.html)

For full features, please install additional packages:

``` r
# install.packages(c("remotes", "visNetwork", "rpymat"))

# Install development version
remotes::install_github("dipterix/ravepipeline")

# Download built-in pipelines
ravepipeline::ravepipeline_finalize_installation()

# Configure Python environment if needed
rpymat::configure_conda()
```

## Run analysis pipelines

_This example requires you to download RAVE demo data, which comes with the full installations._

The built-in pipelines include Power Explorer, a powerful tool to group stimuli for time-frequency analysis and visualization. Here's an example of how to use it on our demo data set:



https://github.com/user-attachments/assets/75fe0f88-58ca-46ee-a831-f22abbaa5343



``` r
library(ravepipeline)

# list all the pipelines
pipeline_list()

# Run power explorer
power_explorer <- pipeline("power_explorer")

# List all runnable pipeline targets
power_explorer$target_table

# set inputs for analysis
power_explorer$set_settings(
  project_name = "demo", 
  subject_code = "DemoSubject", 
  loaded_electrodes = "13-16,24", 
  epoch_choice = "auditory_onset", 
  epoch_choice__trial_starts = -1L, 
  epoch_choice__trial_ends = 2L, 
  reference_name = "default", 
  baseline_settings = list(
    window = list(c(-1, -0.5)),
    scope = "Per frequency, trial, and electrode", 
    unit_of_analysis = "decibel"), 
  analysis_electrodes = "14", 
  first_condition_groupings = list(
    list(label = "audio_visual", conditions = c("known_av", "meant_av", "last_av", "drive_av")),
    list(label = "auditory_only", conditions = c("last_a", "drive_a", "known_a", "meant_a")), 
    list(label = "visual_only", conditions = c("last_v", "drive_v", "known_v", "meant_v"))), 
  condition_variable = "Condition", 
  analysis_settings = list(
    list(label = "AudStart", event = "Trial Onset", 
         time = 0:1, frequency = c(70L, 150L))),
  enable_second_condition_groupings = FALSE, 
  enable_custom_ROI = FALSE, 
  omnibus_includes_all_electrodes = TRUE
)


# Run pipeline to obtain the power of frequency over time
time_freq_data <- power_explorer$run("by_frequency_over_time_data")

# Load up custom pipeline functions
pipeline_functions <- power_explorer$shared_env()

# plot the result
pipeline_functions$plot_by_frequency_over_time(time_freq_data)

# visualize the results
power_explorer$visualize(glimpse = TRUE, aspect_ratio = 10)
```

