tfmtreg_user_defined_r <- function() {
  target_format_register(
    name = "user-defined-r",
    read = function(path, target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {

      ravepipeline <- asNamespace("ravepipeline")
      config <- ravepipeline$load_yaml(file = path)
      if(isTRUE(config$null_value)) {
        return(NULL)
      }

      tryCatch(
        {
          # load <module>/R/serialize.R
          spath <- "./R/serialize.R"
          if(!file.exists(spath)) {
            stop(sprintf("Unable to locate R/serialize.R within the module for unserializing user-defined target [%s].", target_export))
          }
          env <- new.env(parent = baseenv())
          source(spath, local = env, echo = FALSE, chdir = FALSE)
          if(!is.function(env$rave_unserialize)) {
            stop(sprintf("Unable to find function `rave_unserialize` in R/serialize.R for unserializing user-defined target [%s].", target_export))
          }
          path2 <- ravepipeline$target_user_path(target_export = target_export, check = TRUE)
          re <- env$rave_unserialize(path2, target_export)
          return(re)
        },
        error = function(e) {
          traceback(e)
          stop(e$message, call. = FALSE)
        }
      )

    },
    write = function(object, path, target_export = NULL) {

      ravepipeline <- asNamespace("ravepipeline")

      tryCatch(
        {
          # load <module>/R/serialize.R
          spath <- "./R/serialize.R"
          if(!file.exists(spath)) {
            stop(sprintf("Unable to locate R/serialize.R within the module for serializing user-defined target [%s].", target_export))
          }
          env <- new.env(parent = baseenv())
          source(spath, local = env, echo = FALSE, chdir = FALSE)
          if(!is.function(env$rave_serialize)) {
            stop(sprintf("Unable to find function `rave_serialize` in R/serialize.R for serializing user-defined target [%s].", target_export))
          }
          path2 <- ravepipeline$target_user_path(target_export = target_export, check = TRUE)
          path3 <- env$rave_serialize(object, normalizePath(path2, mustWork = FALSE),
                                      target_export)
          if(length(path3) == 1 && !is.na(path3) && is.character(path3) &&
             file.exists(path3)) {
            path2 <- path3
          }
          message(path2)

          # generate signature
          null_value <- FALSE
          if(dir.exists(path2)) {
            fs <- list.files(path2, all.files = FALSE, recursive = TRUE, full.names = TRUE, include.dirs = FALSE, no.. = TRUE)
            data_signature <- lapply(sort(fs), function(f) {
              digest::digest(file = f)
            })
            data_signature <- digest::digest(object = data_signature)
          } else if( file.exists(path2) ){
            data_signature <- digest::digest(file = path2)
          } else {
            null_value <- TRUE
            data_signature <- NULL
          }

          script_signature <- digest::digest(file = spath)
          ravepipeline$save_yaml(
            x = list(
              null_value = null_value,
              script_signature = script_signature,
              data_signature = data_signature
            ),
            file = path,
            sorted = TRUE
          )


        },
        error = function(e) {
          traceback(e)
          stop(e$message, call. = FALSE)
        }
      )
      return()
    }
  )
}
