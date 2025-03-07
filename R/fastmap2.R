# This file is licensed under MIT by Zhengjia Wang

fastmap2 <- function(missing_default = NULL){
  map <- fastmap::fastmap(missing_default = missing_default)
  class(map) <- c('ravepipeline_fastmap2', 'fastmap2', 'list')
  map
}

list_to_fastmap2 <- function(li, map = NULL){
  stopifnot2(is.null(map) || inherits(map, 'fastmap2'), msg = 'map must be either NULL or fastmap2')
  if(is.null(map)){
    map <- fastmap2()
  }
  for(nm in names(li)){
    if(nm != ''){
      map[[nm]] <- li[[nm]]
    }
  }
  map
}

#' @export
`[[.ravepipeline_fastmap2` <- function(x, name){
  name <- as.character(name)
  if( startsWith(name, '@') ){
    .subset2(x, substring(name, 2))
  }else{
    .subset2(x, 'get')(name)
  }
}

#' @export
`$.ravepipeline_fastmap2` <- `[[.ravepipeline_fastmap2`

#' @export
`[[<-.ravepipeline_fastmap2` <- function(x, name, value){
  .subset2(x, 'set')(as.character(name), value)
  return(x)
}

#' @export
`$<-.ravepipeline_fastmap2` <- `[[<-.ravepipeline_fastmap2`

#' @export
`[.ravepipeline_fastmap2` <- function(x, i, j = NULL, ...){
  if(missing(i)) {
    return( .subset2(x, "as_list")(...) )
  } else {
    return( .subset2(x, 'mget')(as.character(unlist(c(i, j, ...)))) )
  }
}

#' @export
`[<-.ravepipeline_fastmap2` <- function(x, i, j = NULL, ..., value){
  i <- unlist(c(i, j, ...))
  # instead of throwing error,
  stopifnot2(length(value) <= 1 || length(value) == length(i),
             msg='value must be the same length as name')
  if( length(value) == length(i) ){
    .subset2(x, 'mset')(.list = structure(as.list(value), names = as.character(i)))
  } else {
    # set for each key
    for(k in i){
      .subset2(x, 'set')(as.character(k), value)
    }
  }

  x
}

#' @export
`names.ravepipeline_fastmap2` <- function(x){
  re <- .subset2(x, 'keys')()
  if(!length(re)){ re <- NULL }
  re
}

#' @export
`print.ravepipeline_fastmap2` <- function(x, ...){
  cat('<Map, size=', .subset2(x, 'size')(),
      ', keys=[', paste(.subset2(x, 'keys')(), collapse = ', '),
      ']>\n', sep = '')
  invisible(x)
}

#' @export
`length.ravepipeline_fastmap2` <- function(x){
  .subset2(x, 'size')()
}

#' @export
as.list.ravepipeline_fastmap2 <- function(x, recursive = FALSE, sorted = FALSE,
                             ...){
  re <- .subset2(x, 'as_list')(sort = sorted)
  if( recursive ){
    for(i in seq_along(re)){
      item <- re[[i]]
      if(inherits(item, "fastmap2")){
        re[[i]] <- as.list(item, recursive = recursive, sorted = sorted, ...)
      }
    }
  }
  re
}
