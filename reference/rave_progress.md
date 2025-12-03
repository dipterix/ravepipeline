# 'RAVE' progress

Automatically displays 'shiny' progress when shiny is present, or text
messages to track progress

## Usage

``` r
rave_progress(
  title,
  max = 1,
  ...,
  quiet = FALSE,
  session = get_shiny_session(),
  shiny_auto_close = FALSE,
  log = NULL
)
```

## Arguments

- title:

  progress title

- max:

  maximum steps

- ...:

  passed to shiny progress

- quiet:

  whether to suppress the progress

- session:

  shiny session

- shiny_auto_close:

  whether to automatically close the progress bar when the parent
  function is closed

- log:

  alternative log function if not default
  ([`message`](https://rdrr.io/r/base/message.html))

## Value

A list of functions to control the progress bar

## Examples

``` r

# Naive example
progress <- rave_progress(title = "progress", max = 10)
#>                                                                                 ▶ [progress]: initializing...
progress$inc("job 1")
#>                                                                                 → [progress]: job 1 (10%)
progress$inc("job 2")
#>                                                                                 → [progress]: job 2 (20%)
progress$close()
#>                                                                                 ■ 

# Within function
slow_sum <- function(n = 11) {
  p <- rave_progress(title = "progress", max = n,
                     shiny_auto_close = TRUE)
  s <- 0
  for( i in seq(1, n) ) {
    Sys.sleep(0.1)
    p$inc(sprintf("adding %d", i))
    s <- s + i
  }
  invisible(s)
}

slow_sum()
#>                                                                                 ▶ [progress]: initializing...
#>                                                                                 → [progress]: adding 1 (9%)
#>                                                                                 → [progress]: adding 2 (18%)
#>                                                                                 → [progress]: adding 3 (27%)
#>                                                                                 → [progress]: adding 4 (36%)
#>                                                                                 → [progress]: adding 5 (45%)
#>                                                                                 → [progress]: adding 6 (55%)
#>                                                                                 → [progress]: adding 7 (64%)
#>                                                                                 → [progress]: adding 8 (73%)
#>                                                                                 → [progress]: adding 9 (82%)
#>                                                                                 → [progress]: adding 10 (91%)
#>                                                                                 → [progress]: adding 11 (100%)
#>                                                                                 ■ 


```
