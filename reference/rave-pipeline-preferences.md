# Pipeline preference management (low-level)

Get, set, and check persistent preference values for 'RAVE' pipelines
and modules. Preferences are stored in a global on-disk store that
survives across R sessions.

## Usage

``` r
pipeline_set_preferences(
  ...,
  .list = NULL,
  .pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  .preference_instance = NULL
)

pipeline_get_preferences(
  keys,
  simplify = TRUE,
  ifnotfound = NULL,
  validator = NULL,
  modes = NULL,
  ...,
  .preference_instance = NULL
)

pipeline_has_preferences(keys, ..., .preference_instance = NULL)
```

## Arguments

- ..., .list:

  for `pipeline_set_preferences`: named values to store, where each name
  is a preference key; for `pipeline_get_preferences`: additional
  arguments forwarded to `validator`

- .pipe_dir:

  the active pipeline directory used to determine the allowed key
  prefix; defaults to the `RAVE_PIPELINE` environment variable or the
  current working directory

- .preference_instance:

  pipeline preference instance: this is automatically filled when
  calling from `pipeline$get_preferences()` When `NULL`, the shared
  on-disk preference store is used automatically

- keys:

  one or more preference key strings following the
  `[prefix].[type].[key]` naming convention

- simplify:

  if `TRUE` (default) and exactly one key is requested, return the value
  directly instead of a length-one named list

- ifnotfound:

  value returned when a requested key is absent or fails validation;
  default is `NULL`

- validator:

  `NULL` or a single-argument function that validates each retrieved
  value; any extra arguments in `...` are forwarded to it. If the
  function signals an error, `ifnotfound` is returned for that key
  instead

- modes:

  `NULL`, or a character vector of expected R
  [`mode`](https://rdrr.io/r/base/mode.html) strings (e.g. `"numeric"`,
  `"character"`) recycled to match the length of `keys`. A stored value
  whose mode does not match is treated as missing and replaced by
  `ifnotfound`

## Value

- `pipeline_set_preferences`:

  Invisibly returns the named list of values that were passed in.

- `pipeline_get_preferences`:

  The preference value(s): a single value when `simplify = TRUE` and one
  key is requested, otherwise a named list with one element per key.

- `pipeline_has_preferences`:

  A logical vector the same length as `keys` indicating which keys
  currently exist in the preference store.

## Details

Preference keys must follow a three-part dot-separated naming convention
`[prefix].[type].[key]`:

- `prefix`:

  Either `"global"` (shared across all modules) or a specific module ID
  such as `"power_explorer"`. When calling `pipeline_set_preferences`
  from within a pipeline, the allowed prefixes are automatically
  restricted to `"global"` and the current pipeline name.

- `type`:

  A category string such as `"graphics"` or `"export"`.

- `key`:

  The individual preference item, e.g. `"use_ggplot"` or
  `"default_format"`.

Valid examples: `"global.graphics.use_ggplot"`,
`"power_explorer.export.default_format"`.

Setting a preference value to `NULL` removes the key from the store.

These functions are low-level: every call repeats the full key, the
default, and the type constraint. Module code should prefer the
declarative methods `define_preference`, `use_preference`, and
`reset_preference` on the
[`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
class, which declare a preference once and then refer to it by a short
name.

## See also

[`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
for the declarative preference methods `define_preference`,
`use_preference`, and `reset_preference`

## Examples

``` r
if (FALSE) { # \dontrun{
# Set preferences (keys use [prefix].[type].[key] convention)
pipeline_set_preferences(
  "global.graphics.use_ggplot" = TRUE,
  "global.graphics.cex" = 1.2
)

# Check whether keys exist
pipeline_has_preferences(
  c("global.graphics.use_ggplot", "global.graphics.cex")
)

# Retrieve a single preference (returns the value directly)
pipeline_get_preferences("global.graphics.cex")

# Retrieve multiple preferences as a named list
pipeline_get_preferences(
  keys = c("global.graphics.use_ggplot", "global.graphics.cex"),
  simplify = FALSE
)

# Return a default when the key is absent
pipeline_get_preferences("global.graphics.missing_key", ifnotfound = FALSE)

# Validate the stored mode; fall back to default on mismatch
pipeline_get_preferences(
  "global.graphics.cex",
  modes = "numeric",
  ifnotfound = 1.0
)

# Remove a preference by setting it to NULL
pipeline_set_preferences("global.graphics.cex" = NULL)
} # }
```
