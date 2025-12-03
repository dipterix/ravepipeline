# 'RAVE' code snippets

Run snippet code

## Usage

``` r
update_local_snippet(force = TRUE)

install_snippet(path)

list_snippets()

load_snippet(topic, local = TRUE)
```

## Arguments

- force:

  whether to force updating the snippets; default is true

- path:

  for installing code snippets locally only; can be an R script, a zip
  file, or a directory

- topic:

  snippet topic

- local:

  whether to use local snippets first before requesting online
  repository

## Value

`load_snippet` returns snippet as a function, others return nothing

## Examples

``` r
# This example script requires running in an interactive session

if(interactive()){

# ---- Example 1: Install built-in pipeline snippets ------------
update_local_snippet(force = TRUE)

# ---- Example 2: Install customized pipeline snippets ----------
snippets <- file.path(
  "https://github.com/rave-ieeg/rave-gists",
  "archive/refs/heads/main.zip",
  fsep = "/"
)
tempf <- tempfile(fileext = ".zip")
utils::download.file(url = snippets, destfile = tempf)

install_snippet(tempf)

}

# ---- List snippets --------------------------------------------

# list all topics
list_snippets()
#> character(0)


# ---- Run snippets as functions --------------------------------

topic <- "image-burn-contacts-to-t1"

# check whether this example can run
# This snippet requires installing package `raveio`
# which is currently not on CRAN (soon it will)

condition_met <- topic %in% list_snippets() &&
  (system.file(package = "raveio") != "")

if( interactive() && condition_met ) {

  snippet <- load_snippet(topic)

  # Read snippet documentation
  print(snippet)


  results <- snippet(
    subject_code = "DemoSubject",
    project_name = "demo",
    save_path = NA,
    blank_underlay = FALSE
  )

  plot(results)
}
```
