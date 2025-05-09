# Abstract Map to store key-value pairs
AbstractMap <- R6::R6Class(
  classname = 'AbstractMap',
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    .id = character(0),

    # Lock file that each queue should have
    # If lock file is locked, then we should wait till the next transaction period
    .lockfile = character(0),
    lock = NULL,

    # Run expr making sure that locker is locked to be exclusive (for write-only)
    exclusive = function(expr, ...) {
      stopifnot2(private$valid, msg = 'Map is not valid')
      custom_locker <- is.function(self$get_locker) && is.function(self$free_locker)
      if(self$has_locker){

        if(custom_locker){
          self$get_locker(...)
        }else{
          private$default_get_locker(...)
        }


        on.exit({
          if(custom_locker){
            self$free_locker()
          }else{
            private$default_free_locker()
          }
        }, add = FALSE, after = FALSE)


        force(expr)

        if(custom_locker){
          self$free_locker()
        }else{
          private$default_free_locker()
        }
        on.exit({}, add = FALSE, after = FALSE)

      } else {
        force(expr)
      }

    },

    default_get_locker = function(timeout = Inf){

      timeout <- as.numeric(timeout)
      if(is.na(timeout)) {
        timeout <- Inf
      }

      simple_lock(name = self$lockfile, timeout = timeout)

    },
    default_free_locker = function(){
      simple_unlock(self$lockfile)
    },

    map = NULL,
    valid = TRUE

  ),
  public = list(

    # By default, queue uses file locker, if you have customized locker, please
    # implement these two methods as functions:
    #   get_locker obtain and lock access (exclusive)
    #   free_locker free the lock
    # private$exclusive will take care the rest
    get_locker = NULL,
    free_locker = NULL,
    has_locker = TRUE,
    missing_default = NULL,



    `@remove` = function(keys){
      not_implemented()
      private$map$remove(keys)
    },
    remove = function(keys){
      private$exclusive({
        self$`@remove`( keys )
      })
    },
    reset = function(...){
      keys <- self$keys(include_signatures = FALSE)
      self$remove( keys )
    },



    keys = function(include_signatures = FALSE){
      not_implemented()

      keys <- private$map$keys()
      if( include_signatures ){
        # Returns two columns: key digest

        keys <- t(sapply(keys, function(k){
          c(k, private$map$get(k)$signature)
        }))
      }

      keys
    },

    size = function(){
      length(self$keys( include_signatures = FALSE ))
    },


    digest = function(signature){
      digest::digest(signature)
    },



    has = function(keys, signature, sig_encoded = FALSE){
      stopifnot2(is.character(keys) || is.null(keys), msg = '`keys` must be a character vector or NULL')
      all_keys <- self$keys(include_signatures = TRUE)

      if(!length(all_keys)){ return(rep(FALSE, length(keys))) }

      has_sig <- !missing(signature)

      if( !sig_encoded && has_sig ){
        signature <- self$digest(signature)
      }

      vapply(keys, function(k){
        sel <- all_keys[,1] == k
        has_key <- any(sel)
        if( has_sig && has_key ){ has_key <- all_keys[sel, 2] == signature }
        has_key
      }, FUN.VALUE = FALSE)
    },




    `@set` = function(key, value, signature){
      not_implemented()
      private$map$set(key = key, value = list(
        signature = signature,
        value = value
      ))
      return( signature )
    },
    set = function(key, value, signature){
      force(value)
      if( missing(signature) ){
        signature <- self$digest( value )
      }else{
        signature <- self$digest( signature )
      }
      private$exclusive({
        self$`@set`(key, value, signature = signature)
      })
      invisible(signature)
    },

    mset = function(..., .list = NULL){
      .list <- c(list(...), .list)
      nms <- names(.list)
      lapply(nms, function(nm){
        self$set(nm, .list[[nm]])
      })
    },



    `@get` = function(key){
      not_implemented()
      return( private$map$get(key)$value )
    },
    get = function(key, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }
      if( self$has( key ) ){
        self$`@get`(key)
      }else{
        missing_default
      }
    },

    mget = function(keys, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }

      has_keys <- self$has( keys )

      re <- lapply(seq_along( keys ), function(ii){
        if( has_keys[[ii]] ){
          self$`@get`(keys[[ ii ]])
        }else{
          missing_default
        }
      })
      names(re) <- keys
      re
    },



    as_list = function(sort = FALSE){
      keys <- self$keys(include_signatures = FALSE)
      if(!length(keys)){
        return(list())
      }
      if( sort ){
        keys <- sort(keys)
      }

      self$mget(keys)
    },

    `@validate` = function(...){
      not_implemented()
    },
    validate = function(...){
      stopifnot2(private$valid, msg = 'Map is invalid/destroyed!')
      private$exclusive({
        self$`@validate`(...)
      })
    },

    # Usually should be called at the end of `initialization` to connect to
    # a database, a folder, or an existing queue
    # you should do checks whether the connection is new or it's an existing
    # queue
    `@connect` = function(...){
      not_implemented()
      private$map <- fastmap::fastmap()
    },
    # thread-safe version. sometimes you need to override this function instead
    # of `@connect`, because `private$exclusive` requires lockfile to be locked
    # If you don't have lockers ready, or need to set lockers during the
    # connection, override this one
    connect = function(...){
      private$exclusive({
        self$`@connect`(...)
      })
    },

    # will be called during Class$new(...), three tasks,
    # 1. set `get_locker` `free_locker` if lock type is not a file
    # 2. set lockfile (if using default lockers)
    # 3. call self$connect
    initialize = function(has_locker = TRUE, lockfile, ...){
      if( has_locker ){
        self$lockfile <- lockfile
      }
      self$connect(...)
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      locker_key(self$lockfile, set_default = FALSE, unset = TRUE)
      private$default_free_locker()
      private$valid <- FALSE
      delayedAssign('.lockfile', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
    }
  ),
  active = list(

    # read-only version of self$id. It's safer than private$.id as the latter
    # one does not always exist
    id = function(){
      if(length(private$.id) != 1){
        private$.id <- rand_string()
      }
      private$.id
    },

    # set/get lock file. Don't call private$.lockfile directly
    lockfile = function(v) {
      if (!missing(v)) {
        private$default_free_locker()
        private$.lockfile <- v
      } else if (!length(private$.lockfile)) {
        private$.lockfile <- rand_string(16)
      }
      private$.lockfile
    },

    is_valid = function(){
      private$valid
    }

  )
)


FileMap <- R6::R6Class(
  classname = 'FileMap',
  inherit = AbstractMap,
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    root_path = character(0),
    header_file = character(0),
    db_dir = character(0)
  ),
  public = list(

    `@remove` = function(keys){
      tbl <- utils::read.csv(private$header_file, header = TRUE, sep = '|',
                      stringsAsFactors = FALSE, na.strings = 'NA', colClasses = 'character')
      if(!length(tbl$Key)){ return(invisible()) }

      enkeys <- sapply(keys, safe_urlencode)
      sel <- tbl$Key %in% enkeys
      if( any(sel) ){
        fs <- tbl$Key[sel]
        tbl <- tbl[!sel, ]
        utils::write.table(tbl, private$header_file, sep = '|', quote = FALSE,
                    row.names = FALSE, col.names = TRUE, append = FALSE)
        # Unlink files
        lapply(file.path(private$db_dir, fs), unlink)
      }
      invisible()
    },
    reset = function(...){
      writeLines('Key|Hash', private$header_file)
      unlink(private$db_dir, recursive = TRUE)
      dir_create2(private$db_dir)
    },

    keys = function(include_signatures = FALSE){
      tbl <- utils::read.csv(private$header_file, header = TRUE, sep = '|',
                      stringsAsFactors = FALSE, na.strings = 'NA', colClasses = 'character')
      if(!length(tbl$Key)){ return(NULL) }

      keys <- sapply(tbl$Key, safe_urldecode)
      if(include_signatures){
        keys <- cbind(keys, tbl$Hash)
      }

      keys
    },

    `@set` = function(key, value, signature){
      # If new key, then no-harm as there is no writing
      self$`@remove`(key)

      # Generate filename from key
      encoded_key <- safe_urlencode(key)
      # signature is already hashed

      # save value
      fpath <- file.path(private$db_dir, encoded_key)
      saveRDS(value, file = fpath)

      utils::write.table(data.frame(
        Key = encoded_key,
        Hash = signature
      ), file = private$header_file, sep = '|', append = TRUE, quote = FALSE,
      row.names = FALSE, col.names = FALSE)

      return( signature )
    },

    `@get` = function(key){
      stop("ravepipeline::FileMap$`@get`: Not yet implemented")
    },
    get = function(key, missing_default){
      ekey <- safe_urlencode(key)
      fpath <- file.path(private$db_dir, ekey)
      if( file.exists(fpath) ){
        readRDS(fpath)
      }else{
        if(missing(missing_default)){ missing_default <- self$missing_default }
        missing_default
      }
    },

    mget = function(keys, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }
      force(missing_default)

      re <- lapply(keys, function(key){
        self$get(key, missing_default)
      })
      names(re) <- keys
      re
    },

    `@validate` = function(...){
      stopifnot2(file.exists(private$header_file), msg = 'Header-file is missing')
      stopifnot2(dir.exists(private$db_dir), msg = 'Database directory is missing')
      stopifnot2(isTRUE(readLines(private$header_file, n = 1) == "Key|Hash"),
                 msg = 'Corruped header file')
    },

    # will be called during Class$new(...), three tasks,
    # 1. set `get_locker` `free_locker` if lock type is not a file
    # 2. set lockfile (if using default lockers)
    # 3. call self$connect
    initialize = function(path){
      path <- dir_create2(path)
      private$root_path <- path
      private$db_dir <- dir_create2(file.path(path, 'MAP-RDSDB'))
      header_file <- file.path(path, 'MAP-RDSHEAD')
      if( !file.exists(header_file) ){
        header_file <- file_create2(header_file)
        writeLines('Key|Hash', con = header_file)
      }
      private$header_file <- header_file
      lockpath <- file.path(path, 'MAP-RDSLOCK')
      if(!file.exists(lockpath)){
        writeLines(rand_string(), lockpath)
      }
      self$lockfile <- readLines(lockpath, n = 1)
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      locker_key(self$lockfile, set_default = FALSE, unset = TRUE)

      lockpath <- file.path(private$root_path, 'MAP-RDSLOCK')
      unlink(lockpath)
      if(dir.exists(private$db_dir)){
        unlink(private$db_dir, recursive = TRUE)
      }
      if(file.exists(private$header_file)){
        unlink(private$header_file)
      }
      unlink(private$root_path, recursive = TRUE, force = FALSE)

      private$valid <- FALSE
      delayedAssign('.lockfile', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
    }
  )
)


rds_map <- function (path = tempfile(pattern = "rave-rdsmap-")) {
  FileMap$new(path = path)
}
