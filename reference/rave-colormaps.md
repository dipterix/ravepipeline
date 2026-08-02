# Built-in 'RAVE' color palettes

Look up, preview, and interpolate the color palettes shipped with
ravepipeline. `DISCRETE_COLORMAPS` holds qualitative palettes, intended
for categorical variables such as electrode groups.
`CONTINUOUS_COLORMAPS` holds sequential and diverging palettes, intended
for numeric heat maps; each of its palettes is interpolated to at least
515 stops the first time it is requested, and the interpolation is
cached for later calls.

## Usage

``` r
DISCRETE_COLORMAPS(
  name,
  seed_colors = NULL,
  preview = missing(name),
  error_if_missing = FALSE
)

CONTINUOUS_COLORMAPS(
  name,
  seed_colors = NULL,
  preview = missing(name),
  error_if_missing = FALSE
)
```

## Arguments

- name:

  palette name; when missing, the entire palette table is returned
  invisibly

- seed_colors:

  optional character vector of colors used in place of the named
  palette. It applies to this call only: the shared palette table is
  left unchanged

- preview:

  whether to draw the palette. The default previews the whole table when
  `name` is missing, and stays quiet otherwise; pass `FALSE` explicitly
  to query the table without drawing

- error_if_missing:

  whether an unknown `name` raises an error instead of emitting a
  warning and falling back to `"default"`

## Value

A character vector of colors in `#RRGGBB` notation; when `name` is
missing, the invisible named list of every palette

## Examples

``` r

names(DISCRETE_COLORMAPS(preview = FALSE))
#>  [1] "default"        "BeautifulField" "Perm4_0"        "Accent"        
#>  [5] "Dark2"          "Paired"         "Pastel1"        "Pastel2"       
#>  [9] "Set1"           "Set2"           "Set3"           "tab10"         
#> [13] "tab20"          "tab20b"         "tab20c"         "tab22"         
#> [17] "tab25"          "turbo20"        "spectral30"    

DISCRETE_COLORMAPS("tab10")
#>  [1] "#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2"
#>  [8] "#7f7f7f" "#bcbd22" "#17becf"

length(CONTINUOUS_COLORMAPS("viridis"))
#> [1] 515

# ad-hoc colors, interpolated but not registered
length(CONTINUOUS_COLORMAPS("scratch", seed_colors = c("#000000", "#FFFFFF")))
#> [1] 515

if(interactive()) {
  DISCRETE_COLORMAPS("Set1", preview = TRUE)
}
```
