save_json <- function(x, con = stdout(), ...,
                      digits = ceiling(-log10(.Machine$double.eps)),
                      pretty = TRUE, serialize = TRUE) {

  ieegio::io_write_json(x = x, con = con, digits = digits, pretty = pretty, serialize = serialize, ...)
}

load_json <- function(con, ..., map = NULL){

  re <- ieegio::io_read_json(con = con)

  if(is.list(re) && !is.null(map)){
    if(is.environment(map)){
      list2env(re, map)
    } else if (inherits(map, 'fastmap2')){
      dipsaus::list_to_fastmap2(re, map)
    } else if (inherits(map, "fastmap")){
      map$mset(.list = re)
    }
  }
  re
}

