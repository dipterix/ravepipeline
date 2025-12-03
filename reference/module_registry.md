# 'RAVE' module registry

Create, view, or reserve the module registry

## Usage

``` r
module_registry(
  title,
  repo,
  modules,
  authors,
  url = sprintf("https://github.com/%s", repo)
)

module_registry2(repo, description)

get_modules_registries(update = NA)

get_module_description(path)

add_module_registry(title, repo, modules, authors, url, dry_run = FALSE)
```

## Arguments

- title:

  title of the registry, usually identical to the description title in
  `'DESCRIPTION'` or `RAVE-CONFIG` file

- repo:

  'Github' repository

- modules:

  characters of module ID, must only contain letters, digits,
  underscore, dash; must not be duplicated with existing registered
  modules

- authors:

  a list of module authors; there must be one and only one author with
  `'cre'` role (see [`person`](https://rdrr.io/r/utils/person.html)).
  This author will be considered maintainer, who will be in charge if
  editing the registry

- url:

  the web address of the repository

- update:

  whether to force updating the registry

- path, description:

  path to `'DESCRIPTION'` or `RAVE-CONFIG` file

- dry_run:

  whether to generate and preview message content instead of opening an
  email link

## Value

a registry object, or a list of registries

## Details

A 'RAVE' registry contains the following data entries: repository title,
name, 'URL', authors, and a list of module IDs. 'RAVE' requires that
each module must use a unique module ID. It will cause an issue if two
modules share the same ID. Therefore 'RAVE' maintains a public registry
list such that the module maintainers can register their own module ID
and prevent other people from using it.

To register your own module ID, please use `add_module_registry` to
validate and send an email to the 'RAVE' development team.

## Examples

``` r
library(ravepipeline)

# create your own registry
module_registry(
  repo = "rave-ieeg/rave-pipelines",
  title = "A Collection of 'RAVE' Builtin Pipelines",
  authors = list(
    list("Zhengjia", "Wang", role = c("cre", "aut"),
         email = "dipterix@rave.wiki")
  ),
  modules = "brain_viewer"
)
#> [RAVE Registry]
#>   Title: A Collection of 'RAVE' Builtin Pipelines
#>   Repository: rave-ieeg/rave-pipelines
#>   URL: https://github.com/rave-ieeg/rave-pipelines
#>   Maintainer: Zhengjia Wang <dipterix@rave.wiki> [cre, aut]
#>   Authors: 
#>   - Zhengjia Wang <dipterix@rave.wiki> [cre, aut]
#>   Modules: [brain_viewer]


if (FALSE) { # \dontrun{

# This example will need access to Github and will open an email link

# get current registries
get_modules_registries(FALSE)

# If your repository is on Github and RAVE-CONFIG file exists
module_registry2("rave-ieeg/rave-pipelines")

# send a request to add your registry
registry <- module_registry2("rave-ieeg/rave-pipelines")
add_module_registry(registry)

} # }

```
