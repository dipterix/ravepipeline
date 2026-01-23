# Create Session-Scoped MCP State Environment

Factory function that creates isolated state environments for MCP tool
sessions. Each session maintains independent pipeline context,
eliminating cross-contamination between concurrent chats or users.

## Usage

``` r
mcptool_state_factory(initialize = NULL, force_init = FALSE, .state = NULL)
```

## Arguments

- initialize:

  Optional function to initialize the session state. Should accept the
  environment as its first argument and perform any required setup

- force_init:

  Logical. If `TRUE`, runs the `initialize` function even if the
  environment appears already initialized. Default is `FALSE`.

- .state:

  Optional existing environment to use. If provided, it will be
  validated and potentially initialized. If `NULL`, creates a new
  environment

## Value

An environment of class `"ravepipeline_mcp_session_state"` with methods
available for key-value storage:

- `reset()`:

  Remove all objects from the map

- `set(key, value)`:

  Set a key-value pair. Returns the value

- `get(key, missing = NULL)`:

  Get a value by key. Returns `missing` if key not found

- `mget(keys, missing = NULL)`:

  Get multiple values by keys

- `has(key)`:

  Check if a key exists. Returns `TRUE` or `FALSE`

- `remove(key)`:

  Remove a key-value pair

- `keys(sort = FALSE)`:

  Get all keys

- `size()`:

  Get the number of items in the map

- `clone()`:

  Return a shallow copy of the map

- `as_list(sort = FALSE)`:

  Return a named list of all key-value pairs

and internal variables:

- `.initialized`:

  Logical flag indicating initialization status

- `.stores`:

  The underlying object that persists the key-values

Parent environment contains read-only shared variables:

- `.platform`:

  Platform information from `.Platform`

- `.interactive`:

  Whether session is interactive

- `.session_info`:

  Session information from
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)

## Examples

``` r
# Create a fresh session state
state1 <- mcptool_state_factory()
state1$set("current_pipeline", "my_pipeline")
state1$.initialized  # TRUE
#> [1] TRUE

# Create with initialization
init_fn <- function(env) {
  env$set("work_path", getwd())
}
state2 <- mcptool_state_factory(initialize = init_fn)
state2$get("work_path")
#> [1] "/home/runner/work/ravepipeline/ravepipeline/docs/reference"

# Reuse existing environment
state3 <- mcptool_state_factory(.state = state1)
identical(state1, state3)  # TRUE
#> [1] TRUE

# Access shared read-only parent variables
get(".platform", state1)
#> $OS.type
#> [1] "unix"
#> 
#> $file.sep
#> [1] "/"
#> 
#> $dynlib.ext
#> [1] ".so"
#> 
#> $GUI
#> [1] "X11"
#> 
#> $endian
#> [1] "little"
#> 
#> $pkgType
#> [1] "source"
#> 
#> $path.sep
#> [1] ":"
#> 
#> $r_arch
#> [1] ""
#> 
get(".session_info", state1)
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C           LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ravepipeline_0.0.3.8
#> 
#> loaded via a namespace (and not attached):
#>  [1] xfun_0.56         bslib_0.9.0       httr2_1.2.2       htmlwidgets_1.6.4
#>  [5] remotes_2.5.0     processx_3.8.6    callr_3.7.6       vctrs_0.7.0      
#>  [9] tools_4.5.2       ps_1.9.1          curl_7.0.0        base64url_1.4    
#> [13] parallel_4.5.2    tibble_3.3.1      fansi_1.0.7       pkgconfig_2.0.3  
#> [17] data.table_1.18.0 secretbase_1.1.1  S7_0.2.1          desc_1.4.3       
#> [21] uuid_1.2-1        lifecycle_1.0.5   compiler_4.5.2    textshaping_1.0.4
#> [25] fontawesome_0.5.3 repr_1.1.7        codetools_0.2-20  httpuv_1.6.16    
#> [29] htmltools_0.5.9   sass_0.4.10       yaml_2.3.12       later_1.4.5      
#> [33] pillar_1.11.1     pkgdown_2.2.0     crayon_1.5.3      jquerylib_0.1.4  
#> [37] whisker_0.4.1     openssl_2.3.4     cachem_1.1.0      mime_0.13        
#> [41] tidyselect_1.2.1  digest_0.6.39     purrr_1.2.1       fastmap_1.2.0    
#> [45] cli_3.6.5         logger_0.4.1      magrittr_2.0.4    base64enc_0.1-3  
#> [49] IRdisplay_1.1     withr_3.0.2       prettyunits_1.2.0 promises_1.5.0   
#> [53] backports_1.5.0   rappdirs_0.3.4    IRkernel_1.3.2    rmarkdown_2.30   
#> [57] igraph_2.2.1      otel_0.2.0        rpymat_0.1.8      pbdZMQ_0.3-14    
#> [61] askpass_1.2.1     ragg_1.5.0        memoise_2.0.1     shiny_1.12.1     
#> [65] evaluate_1.0.5    knitr_1.51        filearray_0.2.0   rlang_1.1.7      
#> [69] downlit_0.4.5     Rcpp_1.1.1        xtable_1.8-4      glue_1.8.0       
#> [73] xml2_1.5.2        coro_1.1.0        rstudioapi_0.18.0 jsonlite_2.0.0   
#> [77] ellmer_0.4.0      R6_2.6.1          systemfonts_1.3.1 targets_1.11.4   
#> [81] fs_1.6.6         
```
