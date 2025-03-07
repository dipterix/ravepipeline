# This file is licensed under MIT by Zhengjia Wang

rave_progress <- function(title, max = 1, ..., quiet = FALSE,
                       session = get_shiny_session(),
                       shiny_auto_close = FALSE, log = NULL){
  if(missing(title) || is.null(title)){ title <- '' }
  if( length(title) > 1 ){ title <- paste(title, collapse = '')}

  if( inherits(session, c('ShinySession', 'session_proxy', 'R6')) ){
    within_shiny <- TRUE
  }else{
    within_shiny <- FALSE
  }

  current <- 0
  closed <- FALSE
  get_value <- function(){ current }
  is_closed <- function(){ closed }
  logger <- function(..., .quiet = quiet, level = 'DEFAULT', bullet = 'play'){
    if(!.quiet){
      if(is.function(log)){
        log(...)
      }else{
        cat2(..., level=level, bullet=bullet, end = "\r", append = TRUE)
      }
    }
  }



  if( quiet || !within_shiny ){
    progress <- NULL
    logger(sprintf("[%s]: initializing...", title), level = 'DEFAULT', bullet = 'play')

    inc <- function(detail, message = NULL, amount = 1, ...){
      stopifnot2(!closed, msg = 'progress is closed')
      quiet <- c(list(...)[['quiet']], quiet)[[1]]
      # if message is updated
      if(!is.null(message) && length(message) == 1){ title <<- message }
      current <<- amount + current
      logger(sprintf("[%s]: %s (%d out of %d)", title, detail, round(current), ceiling(max)),
             level = 'DEFAULT', bullet = 'arrow_right', .quiet = quiet)
    }

    close <- function(message = 'Finished'){
      closed <<- TRUE
      logger(message, level = 'DEFAULT', bullet = 'stop')
    }
    reset <- function(detail = '', message = '', value = 0){
      title <<- message
      current <<- value
    }

  } else {
    progress <- call_pkg_fun("shiny", "Progress", .call_pkg_function = FALSE)$new(session = session, max = max, ...)
    inc <- function(detail, message = NULL, amount = 1, ...){
      if(!is.null(message) && length(message) == 1){ title <<- message }
      current <<- current + amount
      progress$inc(detail = detail, message = title, amount = amount)
      if(is.function(log)){
        logger(sprintf("[%s]: %s (%d out of %d)", title, detail, round(current), ceiling(max)),
               level = 'DEFAULT', bullet = 'arrow_right', .quiet = quiet)
      }
    }
    close <- function(message = 'Finished'){
      if(!closed){
        progress$close()
        closed <<- TRUE
      }
    }
    reset <- function(detail = '', message = '', value = 0){
      title <<- message
      current <<- value
      progress$set(value = value, message = title, detail = detail)
    }
    # get_value <- function() {
    #   try({
    #     re <- progress$getValue()
    #     if(!length(re) || !is.numeric(re)){
    #       re <- current
    #     }
    #     re
    #   }, silent = TRUE)
    # }
    if(shiny_auto_close){
      parent_frame <- parent.frame()
      do.call(
        on.exit, list(substitute(close()), add = TRUE),
        envir = parent_frame
      )
    }
    inc(detail = 'Initializing...', amount = 0)

  }

  return(list(
    .progress = progress,
    inc = inc,
    close = close,
    reset = reset,
    get_value = get_value,
    is_closed = is_closed
  ))
}
