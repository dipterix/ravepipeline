# Show a notification in the editor

Raises a `VSCode` or `Positron` notification from R through the
companion extension, see
[`install_vscode_extension`](http://dipterix.org/ravepipeline/reference/vscode-extension.md).
Returns at once when no live editor window is listening, so it is safe
to call anywhere.

## Usage

``` r
vscode_notify(
  message,
  level = c("info", "warning", "error"),
  actions = NULL,
  wait = FALSE,
  modal = FALSE,
  timeout = NULL
)
```

## Arguments

- message:

  text to show; a length-one character, or a vector whose elements are
  shown on separate lines

- level:

  severity of the notification; one of `'info'`, `'warning'`, or
  `'error'`

- actions:

  optional character vector of button labels

- wait:

  whether to wait for the user to choose an action and return the label
  they clicked; default is `FALSE`, returning as soon as the
  notification has been raised

- modal:

  whether the notification blocks the editor until it is dismissed;
  default is `FALSE`

- timeout:

  seconds to wait for the editor to answer; the default is 5, or 60 when
  `wait=TRUE`

## Value

The label the user clicked when `wait=TRUE`, or `NA` when they dismissed
the notification without choosing; otherwise `TRUE`, invisibly. `NULL`
invisibly when no editor window answered.

## Examples

``` r

if (FALSE) { # \dontrun{

vscode_notify("Pipeline finished")

choice <- vscode_notify("Rebuild the pipeline?", level = "warning",
                        actions = c("Rebuild", "Later"), wait = TRUE)
if (identical(choice, "Rebuild")) { message("rebuilding") }

} # }
```
