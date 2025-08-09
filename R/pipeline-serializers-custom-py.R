tfmtreg_user_defined_python <- function() {
  target_format_register(
    name = "user-defined-python",
    read = new_function2(
      quote_type = "quote",
      env = baseenv(),
      args = alist(path = , target_export = NULL,
                   target_expr = NULL,
                   target_depends = NULL),
      body = quote({
        ravepipeline <- asNamespace("ravepipeline")
        config <- ravepipeline$load_yaml(file = path)
        if(isTRUE(config$null_value)) {
          return(NULL)
        }

        py_error_handler <- function(e) {
          e2 <- asNamespace("reticulate")$py_last_error()
          if(!is.null(e2)) {
            e <- e2
          }
          stop(sprintf(
            "Unable to load user-defined python object [%s]. \nUnserializer reports this error:\n  %s", target_export, paste(e$message, collapse = "\n")
          ), call. = FALSE)
        }


        tryCatch(
          {
            py_module <- ravepipeline$pipeline_py_module(convert = FALSE, must_work = TRUE)
            unserialize_func <- py_module$rave_pipeline_adapters$rave_unserialize
            if(!inherits(unserialize_func, "python.builtin.function")) {
              stop(sprintf("Unable to find unserialization function for user-defined python objects: %s", paste(target_export, collapse = ",")))
            }
            message("Unserializing [", target_export, "] using Python module [", py_module$`__name__`, "]")
            path2 <- ravepipeline$target_user_path(target_export = target_export, check = TRUE)
            re <- unserialize_func(path2, target_export)
            py <- rpymat::import_main(convert = FALSE)
            py[[ target_export ]] <- re
            return(re)

          },
          python.builtin.BaseException = py_error_handler,
          python.builtin.Exception = py_error_handler,
          py_error = py_error_handler,
          error = function(e) {
            traceback(e)
            stop(e$message, call. = FALSE)
          }
        )
      })
    ),
    write = new_function2(
      quote_type = "quote",
      env = baseenv(),
      args = alist(object = , path = , target_export = NULL),
      body = quote({
        ravepipeline <- asNamespace("ravepipeline")

        py_error_handler <- function(e) {
          e2 <- asNamespace("reticulate")$py_last_error()
          if (!is.null(e2)) {
            e <- e2
          }
          stop(
            sprintf(
              "Unable to save user-defined python object [%s]. \nSerializer reports this error:\n  %s",
              target_export,
              paste(e$message, collapse = "\n")
            ),
            call. = FALSE
          )
        }

        tryCatch({
          info_module <- ravepipeline$pipeline_py_info(must_work = TRUE)
          py_module <- ravepipeline$pipeline_py_module(convert = FALSE, must_work = TRUE)
          serialize_func <- py_module$rave_pipeline_adapters$rave_serialize
          if (!inherits(serialize_func, "python.builtin.function")) {
            stop(
              sprintf(
                "Unable to find serialization function for user-defined python objects: %s",
                paste(target_export, collapse = ",")
              )
            )
          }
          script_signature <- digest::digest(file = file.path(
            info_module$target_path,
            sprintf("pipeline_target_%s.py", target_export)
          ))
          message(
            "Serializing [",
            target_export,
            "] using Python module [",
            py_module$`__name__`,
            "]"
          )
          path2 <- ravepipeline$target_user_path(target_export = target_export, check = TRUE)
          message(path2)
          path3 <- serialize_func(object,
                                  normalizePath(path2, mustWork = FALSE),
                                  target_export)
          message(path3)
          if (!is.null(path3) &&
              !inherits(path3, "python.builtin.NoneType")) {
            path3 <- rpymat::py_to_r(path3)
            if (is.character(path3) && length(path3) == 1 &&
                !is.na(path3) && file.exists(path3)) {
              path2 <- path3
            }
          }

          null_value <- FALSE
          if (dir.exists(path2)) {
            fs <- list.files(
              path2,
              all.files = FALSE,
              recursive = TRUE,
              full.names = TRUE,
              include.dirs = FALSE,
              no.. = TRUE
            )
            data_signature <- lapply(sort(fs), function(f) {
              digest::digest(file = f)
            })
            data_signature <- digest::digest(object = data_signature)
          } else if (file.exists(path2)) {
            data_signature <- digest::digest(file = path2)
          } else {
            null_value <- TRUE
            data_signature <- NULL
          }
          ravepipeline$save_yaml(
            x = list(
              null_value = null_value,
              script_signature = script_signature,
              data_signature = data_signature
            ),
            file = path,
            sorted = TRUE
          )
        }, python.builtin.BaseException = py_error_handler, python.builtin.Exception = py_error_handler, py_error = py_error_handler, error = function(e) {
          traceback(e)
          stop(e$message, call. = FALSE)
        })
        return()
      })
    )
  )
}
