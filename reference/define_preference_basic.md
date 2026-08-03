# Declare a preference with a common value constraint

Convenience wrappers around `define_preference` in
[`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
covering the two most common preference shapes: a value restricted to a
fixed set of choices (`define_preference_multichoice`), and a
`TRUE`/`FALSE` flag (`define_preference_logical`). Each builds the
`validator` for you, so a declaration only needs the choices, or the
default.

Re-declaring is cheap and safe: the stored preference metadata is
rewritten only when the declaration actually changes, and a value the
user has already chosen is never overwritten. See
[`define_preference_colormap`](http://dipterix.org/ravepipeline/reference/define_preference_colormap.md)
for palette preferences.

## Usage

``` r
define_preference_multichoice(
  pipeline,
  name,
  choices,
  default = choices[[1]],
  domain = PREFERENCE_DOMAINS,
  partial_match = FALSE,
  verbose = TRUE,
  force = FALSE
)

define_preference_logical(
  pipeline,
  name,
  default,
  domain = PREFERENCE_DOMAINS,
  verbose = TRUE,
  force = FALSE
)
```

## Arguments

- pipeline:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance

- name:

  preference name; may only contain letters, digits, underscores, or
  dashes

- choices:

  character vector of the values the preference may take

- default:

  value used whenever the preference is unset; the first element of
  `choices` for `define_preference_multichoice`, and a length-one,
  non-missing logical for `define_preference_logical`

- domain:

  preference domain, one of `"default"`, `"graphics"`, `"analysis"`, or
  `"export"`

- partial_match:

  whether a unique abbreviation of a choice is accepted; default is
  false. When true, the abbreviation is stored as it was given, and
  reading it back with `pipeline$use_preference` expands it to the full
  choice

- verbose:

  whether to emit trace-level logs; default is true

- force:

  whether to re-declare even when the stored declaration is already
  current; default is false. See `define_preference` in
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md):
  declaring is skipped when the recorded version is up to date, so these
  helpers can be called on every launch without touching the disk. Set
  to `TRUE` only while developing

## Value

Invisibly, a list with the stored value (`preference_value`), whether
the declaration was rewritten (`metadata_updated`), and the preference
`metadata`

## Examples

``` r

library(ravepipeline)
if(interactive() && length(pipeline_list()) > 0) {
  pipeline <- pipeline("power_explorer")

  res <- define_preference_multichoice(
    pipeline, name = "annotation_style",
    choices = c("none", "channel", "label"),
    domain = "graphics", partial_match = TRUE
  )
  res$preference_value                            # "none", the default

  pipeline$use_preference("annotation_style", value = "label")

  # a unique abbreviation is accepted and expands on read
  pipeline$use_preference("annotation_style", value = "chan")

  # anything else is rejected, and the stored value is left alone
  try({ pipeline$use_preference("annotation_style", value = "nope") })

  define_preference_logical(pipeline, name = "show_legend", default = TRUE)
  pipeline$use_preference("show_legend")
}
```
