# This file is licensed under MIT by Zhengjia Wang

locker_key <- function (name, set_default = FALSE, unset = FALSE) {
  if (length(name) != 1) {
    return()
  }
  .locker_keys <- get0(".locker_keys")
  name <- sprintf("%s-%s", session_uuid(), name)
  lkey <- .locker_keys$get(name)
  if (unset) {
    .locker_keys$remove(name)
  } else if (is.null(lkey) && set_default) {
    lkey <- sprintf("%s%s", session_uuid(), rand_string(21))
    lkey <- sprintf("%s-%s", lkey, digest::digest(lkey))
    .locker_keys$set(name, lkey)
  }
  lkey
}

validate_lock <- function (lock_path) {
  if(!file.exists(lock_path)) { return(FALSE) }
  suppressWarnings({
    try({
      lkey <- readLines(lock_path)
      lkey <- strsplit(lkey, "-")[[1]]
      if (length(lkey) == 2 && identical(digest::digest(lkey[[1]]), lkey[[2]])) {
        return(TRUE)
      }
    }, silent = TRUE)
  })
  return(FALSE)
}

validate_key <- function (lock_path, key) {
  suppressWarnings({
    try({
      lkey <- readLines(lock_path)
      if (identical(lkey, key)) {
        return(TRUE)
      }
    }, silent = TRUE)
  })
  return(FALSE)
}

simple_lock <- function (name, timeout = 10, exclusive = TRUE) {

  # message("Locking... ", name)
  debug <- function(...) {
    # cat(..., "\n")
  }


  # DIPSAUS DEBUG START
  # path <- tempfile()
  # generator <- ravepipeline:::FileMap
  #
  # self <- generator$new(path = path)
  # private <- self$.__enclos_env__$private
  # self$reset()
  # name = self$lockfile
  # timeout = 10
  timeout <- as.numeric(timeout)
  if (is.na(timeout)) {
    timeout <- 0
  }
  name <- gsub("[^a-zA-Z0-9]", "", name)
  lkey <- locker_key(name, set_default = TRUE)

  debug(sprintf("[timeout=%.1f] using name [%s] \n\t--> key [%s]", timeout, name, lkey))

  root_path <- ravepipeline_cache_dir("file_locks")
  lock_path <- file.path(root_path, name)
  if (!dir.exists(root_path)) {
    dir.create(root_path, showWarnings = FALSE, recursive = TRUE)
  }
  lock_valid <- validate_lock(lock_path)

  debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, lock_valid))

  if (!lock_valid) {
    writeLines(lkey, con = lock_path)
    Sys.sleep(0.001)
    lock_valid <- validate_lock(lock_path)

    debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, lock_valid))

    if (!lock_valid) {
      writeLines(lkey, con = lock_path)
      Sys.sleep(0.001)
      lock_valid <- validate_lock(lock_path)

      debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, lock_valid))

    }
    if (lock_valid && validate_key(lock_path, lkey)) {

      debug(sprintf("[timeout=%.1f] valid key, locked successful", timeout))

      return(TRUE)
    }
  } else {
    try({
      if (validate_key(lock_path, lkey)) {

        debug(sprintf("[timeout=%.1f] valid key, locked successfully", timeout))

        return(TRUE)
      }
    }, silent = TRUE)
  }
  if (timeout <= 0) {

    debug(sprintf("[timeout=%.1f] timeout, returning FALSE (error out)", timeout))

    return(FALSE)
  }
  while (timeout > 0) {
    wait_time <- stats::runif(1, min = min(timeout, 0.1),
                              max = min(timeout, 0.2))
    Sys.sleep(wait_time)
    timeout <- timeout - wait_time
    lock_valid <- validate_lock(lock_path)

    debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, lock_valid))

    if (!lock_valid) {
      writeLines(lkey, con = lock_path)
      Sys.sleep(0.001)
      timeout <- timeout - 0.001
      lock_valid <- validate_lock(lock_path)

      debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, lock_valid))

      if (lock_valid && validate_key(lock_path, lkey)) {
        debug(sprintf("[timeout=%.1f] valid key, locked successful", timeout))
        return(TRUE)
      }
    } else {
      try({
        if (validate_key(lock_path, lkey)) {

          debug(sprintf("[timeout=%.1f] valid key, locked successfully", timeout))

          return(TRUE)
        }
      }, silent = TRUE)
    }
  }
  lock_valid <- validate_lock(lock_path)

  debug(sprintf("[timeout=%.1f] double-validated key [valid=%s]", timeout, lock_valid))

  return(lock_valid && validate_key(lock_path, lkey))
}


simple_unlock <- function (name, timeout = 10, exclusive = TRUE) {

  # message("Unlocking... ", name)
  debug <- function(...) {
    # cat(..., "\n")
  }

  timeout <- as.numeric(timeout)
  if (is.na(timeout)) {
    timeout <- 0
  }
  name <- gsub("[^a-zA-Z0-9]", "", name)
  cache_dir <- ravepipeline_cache_dir()
  root_path <- file.path(cache_dir, "file_locks")
  lock_path <- file.path(root_path, name)
  unlocked <- function() {
    locker_key(name, set_default = FALSE, unset = TRUE)
    TRUE
  }
  if (!file.exists(lock_path)) {
    debug(sprintf("[timeout=%.1f] locker does not exists, releasing", timeout))
    return(unlocked())
  }
  locker_valid <- validate_lock(lock_path)
  if (!locker_valid) {

    debug(sprintf("[timeout=%.1f] locker is invalid and should be deleted, releasing", timeout))

    return(TRUE)
  }
  lkey <- locker_key(name, set_default = FALSE)
  try({
    if (validate_key(lock_path, lkey)) {

      debug(sprintf("[timeout=%.1f] locker is matched, releasing", timeout))

      unlink(lock_path)
      remove_empty_dir(cache_dir, recursive = TRUE)
      return(unlocked())
    }
  }, silent = TRUE)
  while (timeout > 0) {
    wait_time <- stats::runif(1, min = min(timeout, 0.1),
                              max = min(timeout, 0.2))
    Sys.sleep(wait_time)
    timeout <- timeout - wait_time
    locker_valid <- validate_lock(lock_path)

    debug(sprintf("[timeout=%.1f] validated key [valid=%s]", timeout, locker_valid))

    if (!locker_valid) {
      debug(sprintf("[timeout=%.1f] locker is invalid and should be deleted, releasing", timeout))
      return(unlocked())
    }
    try({
      if (validate_key(lock_path, lkey)) {
        debug(sprintf("[timeout=%.1f] locker is matched, releasing", timeout))
        unlink(lock_path)
        remove_empty_dir(cache_dir, recursive = TRUE)
        return(unlocked())
      }
    }, silent = TRUE)
  }

  debug(sprintf("[timeout=%.1f] timeout!", timeout))
  return(FALSE)
}
