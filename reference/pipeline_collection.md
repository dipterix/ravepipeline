# Combine and execute pipelines

Experimental, subject to change in the future.

## Usage

``` r
pipeline_collection(root_path = NULL, overwrite = FALSE)
```

## Arguments

- root_path:

  directory to store pipelines and results

- overwrite:

  whether to overwrite if `root_path` exists; default is false, and
  raises an error when `root_path` exists

## Value

A
[`PipelineCollections`](http://dipterix.org/ravepipeline/reference/PipelineCollections.md)
instance
