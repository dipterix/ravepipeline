## Version 0.0.2 (current)

R CMD check results

0 errors | 0 warnings | 1 note

```r
The BugReports field in DESCRIPTION has
  https://github.com/orgs/rave-ieeg/discussions
which should likely be
  https://github.com/orgs/rave-ieeg/discussions/issues
instead.
```

The address is correct and there is no need to add "issues"" to the path.


## Version 0.0.1

R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


Addressing the CRAN comments:


```
please remove the single quotes around citation("ravepipeline").
```

Thanks, they are removed

```
Please reduce the length of the title to less than 65 characters.
```

Thanks, changed title to "Reproducible Pipeline Infrastructure for Neuroscience" (53 characters)


```
We see: if(interactive()) within \dontrun :
  man/ravepipeline_finalize_installation.Rd

\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest.

Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}.

For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

```

Thanks, removed `if(interactive())` and only keeping `dontrun` since the script will prompt users to install additional packages (depending on requirements of third-party pipeline code).


```
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.
e.g.:
...
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
...

...
oldwd <- getwd() # code line i
on.exit(setwd(oldwd)) # code line i+1
...
setwd(...) # somewhere after
...
e.g.: -> R/aaa.R; R/class-PipelineTools.R; R/options.R; R/parallel.R; R/pipeline-install.R; R/pipeline-knitr.R; R/pipeline-run.R; R/pipeline-tools.R; R/pkginstall.R; R/zzz.R;

If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
For more details: <https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>

```

For `options`:

I have tried my best to add `on.exit` to these functions. All the changes to `options` are handled with `on.exit` except for `R/options.R`, in which the functions are:

1. designed to alter and persist the options (so no expect to call `on.exit` here)
2. only affecting package `dipsaus` and `threeBrain`, and I'm the maintainer of these two packages (and they mainly serve the 'RAVE' project as well).
3. section 'Side-Effects' has been added to the document to warn users that these options are changed.

All other changes to the options are fixed to ensure the options get reset at or even before the enclosure ends.


For `setwd`:

All the changes to the working directories have been reset at or before the ending of enclosures, except for function `activate_pipeline` in `R/pipeline-tools.R`, a special case. Please allow me to describe the case and the safety measures:

With `activate_pipeline(..., debug=FALSE)` (default and expected usage), the working directory is reset at its parent frame (see below). The function is never called at the top level. To ensure that working directory is reset correctly, I added test `tests/testthat/test-workdir.R`.

``` r
parent_function <- function() {
  # change working dirctory
  activate_pipeline()
  
  ... do something under the context
  
  # automatically reset at the end
}

```


With `activate_pipeline(..., debug=TRUE)`, the working directory is not reset, however, 
  
  1. This function (`activate_pipeline`) is not exported. Users must explicitly call `ravepipeline:::activate_pipeline` (with `:::`) to enable the debug mode. When users call `activate_pipeline`, it means that users want to debug the pipelines, and expect this behavior (changing working directory)
  2. the working directory is ONLY allowed under `interactive()` mode (error will be raised if not)
  3. the function raises an warning `warning(sprintf("Debugging a pipeline. ...` even under interactive session. The warning provides instructions to reset working directory.



```
Please do not modifiy the .GlobalEnv. This is not allowed by the CRAN policies.
```

Thanks, the code that changes global environment has been removed. Using `uuid` package instead. In addition, all the calls with `globalenv()` is wrapped with `new.env(parent = globalenv())` to avoid accidentally writing to global environment.


```
Please do not install packages in your functions, examples or vignette. This can make the functions,examples and cran-check very slow.
For more details: <https://contributor.r-project.org/cran-cookbook/code_issues.html#installing-software>
```

Thanks, packages should not install in testing & examples now. In addition, the following safety measures have been added:

* Function `get_remotes_fun` in `R/pkginstall.R` will raise errors when using `remotes` package in the checking environment.
* Additional R check with `nosuggests` results in no errors/warnings/notes; see https://github.com/dipterix/ravepipeline/actions/runs/13774856744/job/38521764803
