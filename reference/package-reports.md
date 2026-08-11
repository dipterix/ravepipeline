# Discover and build reports shipped with an R package

`pkg_available_reports` lists the `R Markdown` reports that a package
ships; `pkg_build_report` renders one of them to a self-contained `HTML`
file.

## Usage

``` r
pkg_available_reports(package)

pkg_build_report(
  report_name,
  package,
  ...,
  theme = "flatly",
  code_folding = "hide",
  self_contained = TRUE,
  quiet = FALSE,
  build_path = tempfile(),
  params = list()
)
```

## Arguments

- package:

  character; name of an installed package to look in.

- report_name:

  character; name of the report to build, as returned by
  `pkg_available_reports`. Matched ignoring case.

- ...:

  additional output options passed to
  [`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html);
  these override the defaults set by this function.

- theme:

  character; `Bootswatch` theme name, default `"flatly"`.

- code_folding:

  character; whether code blocks start folded, one of `"hide"`
  (default), `"show"`, or `"none"`.

- self_contained:

  logical; whether to bundle all dependencies into a single `HTML` file,
  default `TRUE`.

- quiet:

  logical; whether to suppress the rendering progress messages, default
  `FALSE`.

- build_path:

  directory in which the report is built; defaults to a fresh
  [`tempfile`](https://rdrr.io/r/base/tempfile.html). **Any existing
  file or directory at this path is deleted recursively** before the
  build starts, so do not point it at a location whose contents matter.

- params:

  named list of report parameters, only forwarded when non-empty; the
  `Rmd` file must declare a matching `params` entry in its `YAML`
  header.

## Value

`pkg_available_reports`: a character vector of report names, or
`character(0)` when the package ships no reports.

`pkg_build_report`: the normalized path to the generated `HTML` file
inside `build_path`.

## Details

Reports live under the `reports` directory of an installed package
(`inst/reports` in its sources). Every `Rmd` file found there, including
files in sub-directories, is a report. Its name is the path relative to
`reports` with the extension removed, so `inst/reports/qc/summary.Rmd`
is the report `"qc/summary"`. Names are matched ignoring case.

Two optional style sheets are picked up automatically: a shared
`common.css` at the root of `reports`, and a `<report>_styles.css`
sitting next to the `Rmd` file. Both are copied into `build_path` and
passed to
[`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
as its `css` argument.

The report is knitted in a new environment whose parent is the one above
the global environment. Objects the caller left in the global
environment are therefore invisible to the report, and so are packages
attached after the render begins: a report that needs knitr should call
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) rather than
rely on [`library(knitr)`](https://yihui.org/knitr/) in a setup chunk.

## Examples

``` r

# This package ships no reports, hence `character(0)`
pkg_available_reports("ravepipeline")
#> character(0)

if (FALSE) { # \dontrun{

# Requires a package that ships reports, plus a working `pandoc`
pkg_available_reports("ravecore")

pkg_build_report("project-snapshot", package = "ravecore")

} # }
```
