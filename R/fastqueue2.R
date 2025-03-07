# This file is licensed under MIT by Zhengjia Wang

fastqueue2 <- function(init = 20L, missing_default = NULL){
  queue <- fastmap::fastqueue(init = init, missing_default = missing_default)
  head <- 0
  count <- 0
  i <- NA
  ev <- new.env(parent = environment(.subset2(queue, 'as_list')))
  queue$at <- new_function2(args = alist(i=), {
    if(is.na(i) || i < 1L || i > count){
      stop("subscript out of bounds")
    }
    q[[head - count + i]]
  }, env = ev)
  queue$mat <- new_function2(args = alist(i=), {
    q[head - count + i]
  }, env = ev)
  class(queue) <- c('ravepipeline_fastqueue2', 'fastqueue2', 'list')
  queue
}


list_to_fastqueue2 <- function(li, queue = NULL){
  stopifnot2(is.null(queue) || inherits(queue, 'fastqueue2'), msg = 'queue must be either NULL or fastqueue2')
  if(is.null(queue)){
    queue <- fastqueue2()
  }
  # for(ii in seq_along(li)){
  #   .subset2(queue, "add")(li[[ii]])
  # }
  if(!is.list(li)){
    li <- as.list(li)
  }
  if(length(li)){
    .subset2(queue, "madd")(.list = li)
  }
  queue
}


#' @export
`[[.ravepipeline_fastqueue2` <- function(x, i){
  .subset2(x, "at")(i)
}

#' @export
`[[<-.ravepipeline_fastqueue2` <- function(x, i, value){
  stop("Cannot set index of a queue. Use `$add` or `$madd` method.")
  return(x)
}

#' @export
`$<-.ravepipeline_fastqueue2` <- `[[<-.ravepipeline_fastqueue2`

#' @export
`[.ravepipeline_fastqueue2` <- function(x, i, j = NULL, ...){
  if(missing(i)) {
    return( .subset2(x, "as_list")(...) )
  } else {
    return( .subset2(x, "mat")(unlist(c(i, j, ...))) )
  }

}

#' @export
`[<-.ravepipeline_fastqueue2` <- function(x, i, j = NULL, ..., value){
  stop("Cannot subset-assign a queue. Use `$add` or `$madd` method.")
  x
}

#' @export
`print.ravepipeline_fastqueue2` <- function(x, ...){
  cat('<Queue, size=', .subset2(x, 'size')(), '>\n', sep = '')
  invisible(x)
}

#' @export
`length.ravepipeline_fastqueue2` <- function(x){
  .subset2(x, 'size')()
}

#' @export
as.list.ravepipeline_fastqueue2 <- function(x, ...){
  .subset2(x, 'as_list')()
}
