tfmtreg_filearray <- function() {

  # Running this function require installing `filearray`
  # this is checked by `require_package("filearray")`

  target_format_register(
    "filearray",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      require_package("filearray")
      target_expr <- substitute(target_expr)
      data <- readRDS(path)
      if(!is.list(data) || !isTRUE(
        data$type %in% c("asis", "array", "filearray")
      )) {
        stop("Cannot restore filearray/array from the saved target file.")
      }

      if(data$type == "asis") {
        return(data$data)
      }
      abspath <- c(
        data$data$abspath,
        file.path(dirname(path), data$data$basename),
        target_user_path(paste0(target_export, ".FILEARRAY.DATA"))
      )
      abspath <- as.character(abspath[!is.na(abspath)])
      abspath <- abspath[dir.exists(abspath)]

      object <- tryCatch({
        if(length(abspath)) {
          abspath <- abspath[[1]]
          object <- filearray::filearray_load(
            filebase = abspath,
            mode = ifelse(identical(data$data$mode, "readwrite"),
                          "readwrite", "readonly")
          )
          if(data$type == "array" && !isTRUE(object$get_header("lazy_load"))) {
            object <- object[drop = isTRUE(data$data$auto_drop)]
          }
        } else {
          stop("Invalid filearray path")
        }
        object
      }, error = function(e) {
        message(sprintf("Cannot restore [%s]... Re-generate the target", target_export))
        # failed to load filearray, retry
        env <- new.env(parent = globalenv())
        lapply(target_depends, function(name) {
          delayedAssign(name, {
            targets::tar_read_raw(name = name)
          }, assign.env = env)
          return()
        })
        eval(target_expr, envir = env)
      })

      return(object)
    },
    write = function(object, path, target_export = NULL) {
      # object = NULL or zero length
      require_package("filearray")

      filebase <- normalizePath(
        target_user_path(paste0(target_export, ".FILEARRAY.DATA")),
        mustWork = FALSE
      )
      if(!length(object)) {
        data <- list(
          type = "asis",
          data = object
        )
        saveRDS(data, path)
        return()
      }
      if (inherits(object, "FileArray")) {
        # if(object$get_header("targets_nocopy", FALSE)) {
        #   # the cache is handled by RAVE, do not copy
        # }
        filebase_orig <- normalizePath(object$.filebase, mustWork = FALSE)
        if(filebase_orig != filebase) {
          filebase_dir <- dirname(filebase)
          if(!dir.exists(filebase_dir)) {
            dir.create(filebase_dir, recursive = TRUE,
                       showWarnings = FALSE)
          }
          if(file.exists(filebase)) {
            unlink(filebase, recursive = TRUE)
          }
          file.copy(from = filebase_orig, to = filebase_dir,
                    copy.date = TRUE, recursive = TRUE,
                    overwrite = TRUE)
          orig_name <- basename(filebase_orig)
          file_move(
            file.path(filebase_dir, orig_name),
            filebase
          )
        }
        data <- list(
          type = "filearray",
          data = list(
            abspath = filebase,
            basename = basename(filebase),
            mode = object$.mode
          )
        )
        saveRDS(data, path)
        return()
      }

      if(!is.array(object) && !is.matrix(object) &&
         !is.vector(object)) {
        stop("To save/load as `filearray`, the object must be zero length, a vector/matrix/array, or a `filearray`")
      }

      mode <- storage.mode(object)
      if(!mode %in% c(
        "integer", "double", "complex", "logical", "raw")) {
        stop("To save/load as `filearray`, the object must be numeric/complex/logical/raw")
      }


      if(is.array(object) || is.matrix(object)) {
        dm <- dim(object)
        auto_drop <- FALSE
      } else {
        dm <- c(length(object), 1L)
        auto_drop <- TRUE
      }
      headers <- attr(object, "filearray_headers")
      headers2 <- NULL
      if(is.list(headers)) {
        nms <- names(headers)
        if(length(nms)) {
          headers2 <- headers[!nms %in% c(
            "", "filebase", "mode", "dimension", "type",
            "initialize", "symlink_ok")]
        }
      }
      signature <- headers2$signature
      if(!length(signature)) {
        signature <- digest::digest(object)
        headers2$signature <- signature
      }
      if(!is.function(headers2$on_missing)) {
        headers2$on_missing <- function(arr) {
          arr[] <- object
          dimnames(arr) <- dimnames(object)
          arr
        }
      }
      args <- c(list(
        filebase = filebase, mode = "readwrite",
        dimension = dm, type = mode,
        initialize = FALSE, symlink_ok = FALSE
      ), headers2)
      do.call(filearray::filearray_load_or_create, args)

      data <- list(
        type = "array",
        data = list(
          abspath = filebase,
          basename = basename(filebase),
          auto_drop = auto_drop,
          mode = "readonly"
        )
      )
      saveRDS(data, path)
    },
    marshal = function(object, target_export = NULL, target_depends = NULL) {

      # object = NULL or zero length
      require_package("filearray")
      if (inherits(object, "FileArray")) {
        # if(object$get_header("targets_nocopy", FALSE)) {
        #   # the cache is handled by RAVE, do not copy
        # }
        filebase_orig <- normalizePath(object$.filebase, mustWork = FALSE)
        return(structure(filebase_orig, filemode = object$.mode))
      }
      return(object)
    },
    unmarshal = function(object, target_export = NULL) {
      if(is.character(object)) {
        ns <- require_package("filearray", return_namespace = TRUE)

        filemode <- match.arg(attr(object, "filemode"), c("readonly", "readwrite"))
        filebase <- normalizePath(object, mustWork = TRUE)
        object <- ns$filearray_load(filebase = filebase, mode = filemode)
      }
      object
    }
  )
}
