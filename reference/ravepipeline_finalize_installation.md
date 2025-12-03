# Download 'RAVE' built-in pipelines and code snippets

The official built-in pipeline repository is located at
<https://github.com/rave-ieeg/rave-pipelines>; The code snippet
repository is located at <https://github.com/rave-ieeg/rave-gists>.

## Usage

``` r
ravepipeline_finalize_installation(
  upgrade = c("ask", "always", "never", "config-only", "data-only"),
  async = FALSE,
  ...
)
```

## Arguments

- upgrade:

  rules to upgrade dependencies; default is to ask if needed

- async:

  whether to run in the background; ignore for now

- ...:

  ignored; reserved for external calls.

## Value

A list built-in pipelines will be installed, the function itself returns
nothing.

## Examples

``` r
if (FALSE) { # \dontrun{

# This function requires connection to the Github, and must run
# under interactive session since an user prompt will be displayed

ravepipeline_finalize_installation()

} # }
```
