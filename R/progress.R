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


rave_progress_dev <- function(title, max = NA, ..., quiet = FALSE,
                          session = get_shiny_session(),
                          shiny_auto_close = TRUE) {

  env <- new.env(parent = globalenv())
  env$rave_progress_message <- title
  env$rave_progress_detail <- NULL
  env$rave_progress_started <- FALSE
  env$rave_progress_value <- 0

  if(is.null(session)) {
    format <- "{cli::pb_spin} { paste(rave_progress_message, collapse = '') } [{cli::pb_current}/{cli::pb_total}] { paste(rave_progress_detail, collapse = '') }"
    format_done <- "{ paste(c('\r', rep(' ', getOption('width')), '\rDone.'), collapse = '') }"

    # progress_title <- title
    print_details <- function(...) {}
  } else {
    format <- NULL
    format_done <- NULL

    # progress_title <- ""
    print_details <- function(...) {
      details <- trimws(paste(env$rave_progress_detail, collapse = ""))
      if(nzchar(details)) {
        try({
          cli::cli_progress_output(
            text = details,
            id = id,
            .envir = env
          )
        }, silent = TRUE)
      }
    }

  }

  id <- cli::cli_progress_bar(
    name = title,
    status = "Initializing...",
    type = "tasks",
    total = max,
    format = format,
    format_done = format_done,
    # format_failed = "Progress failed",
    auto_terminate = FALSE,
    .auto_close = shiny_auto_close,
    .envir = env
  )

  progress_update <- function(inc) {
    if(is.null(session)) {
      cli::cli_progress_update(
        inc = inc,
        id = id,
        .envir = env,
        status = paste(env$rave_progress_message, collapse = "")
      )
    } else {
      cli::cli_progress_update(
        inc = inc,
        id = id,
        .envir = env,
        status = ""
      )
    }

  }

  return(list(
    id = id,
    inc = function(detail, message = NULL, amount = 1, ...){
      env$rave_progress_value <- env$rave_progress_value + amount
      if(length(message)) {
        env$rave_progress_message <- trimws(paste(c(message, "\r"), collapse = ""))
      } else if(!env$rave_progress_started) {
        env$rave_progress_message <- title
      }
      env$rave_progress_detail <- trimws(paste(c(detail, "\r"), collapse = ""))
      if( env$rave_progress_value > 0 ) {
        env$rave_progress_started <- TRUE
      }
      progress_update(inc = amount)
      if( env$rave_progress_value > 0 ) {
        env$rave_progress_started <- TRUE
      }
      print_details()
    },
    close = function(message = 'Finished'){
      if( env$rave_progress_started ) {
        env$rave_progress_detail <- NULL
        cli::cli_progress_done(id = id, .envir = env)
      }
      env$rave_progress_started <- FALSE
      env$rave_progress_value <- max
    },
    reset = function(detail = NULL, message = '', value = 0){
      env$rave_progress_value <- value
      env$rave_progress_started <- TRUE
      env$rave_progress_message <- message
      env$rave_progress_detail <- NULL
      cli::cli_progress_update(
        inc = 0,
        id = id,
        .envir = env,
        set = value,
        status = paste(message, collapse = "")
      )
      print_details()
    },
    get_value = function() {
      env$rave_progress_value
    },
    is_closed = function() {
      return(env$rave_progress_started)
    }
  ))


}
