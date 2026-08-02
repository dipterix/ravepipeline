# Declare a color palette preference

Convenience wrappers around `pipeline$define_preference` that declare a
palette preference in the `"graphics"` domain, validate it against the
matching table in
[`rave-colormaps`](http://dipterix.org/ravepipeline/reference/rave-colormaps.md),
and resolve the stored palette name to the colors themselves. Use
`define_preference_discrete_colormap` for categorical variables and
`define_preference_continuous_colormap` for numeric heat maps.

Re-declaring is cheap and safe: the stored preference metadata is
rewritten only when the declaration actually changes, and a user's
chosen palette is never overwritten.

## Usage

``` r
define_preference_discrete_colormap(
  pipeline,
  default = "default",
  verbose = TRUE
)

define_preference_continuous_colormap(
  pipeline,
  default = "default",
  verbose = TRUE
)
```

## Arguments

- pipeline:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance

- default:

  palette name used whenever the preference is unset; must be a valid
  name for the corresponding table

- verbose:

  whether to emit trace-level logs; default is true

## Value

Invisibly, a list with the stored palette name (`preference_value`),
whether the declaration was rewritten (`metadata_updated`), and the
preference `metadata`. Read the colors themselves with
`pipeline$use_preference`, which runs the declared `getter`

## Examples

``` r

library(ravepipeline)
if(interactive() && length(pipeline_list()) > 0) {
  pipeline <- pipeline("power_explorer")

  res <- define_preference_discrete_colormap(pipeline)
  res$preference_value    # the palette name

  # the colors, plus the name they came from
  colors <- pipeline$use_preference("discrete_colormap")
  attr(colors, "preference_value")

  pipeline$use_preference("discrete_colormap", value = "tab10")
}
```
