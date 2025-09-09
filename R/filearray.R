#' @title 'R6' wrapper for \code{'FileArray'}
#' @description
#' Wrapper for better serialization (check 'See also')
#' @seealso \code{\link{RAVESerializable}} \code{\link{rave-serialize-refhook}}
#' @export
RAVEFileArray <- R6::R6Class(
  classname = "RAVEFileArray",
  portable = TRUE,
  inherit = RAVESerializable,
  private = list(
    impl = NULL,

    finalize = function(...) {
      if(self$temporary && self$valid && identical(private$impl$.mode, "readwrite")) {
        private$impl$delete()
      }
    }
  ),
  public = list(

    #' @field temporary whether this file array is to be upon garbage collection;
    #' default is false. The file array will be deleted if the
    #' temporary flag is set to true and the array mode is \code{'readwrite'}
    temporary = FALSE,

    #' @description Serialization helper, convert the object to a descriptive list
    #' @param ... ignored
    `@marshal` = function(...) {
      list(
        namespace = "ravepipeline",
        r6_generator = "RAVEFileArray",
        data = list(
          filebase = private$impl$.filebase,
          mode = private$impl$.mode
        )
      )
    },

    #' @description Serialization helper, convert the object from a descriptive list
    #' @param object serialized list
    #' @param ... ignored
    `@unmarshal` = function(object, ...) {

      stopifnot(identical(object$namespace, "ravepipeline"))
      stopifnot(identical(object$r6_generator, "RAVEFileArray"))

      # `ravepipeline` might not be loaded yet...
      ravepipeline <- asNamespace("ravepipeline")
      x <- filearray::filearray_load(filebase = object$data$filebase, mode = object$data$mode)
      ravepipeline$RAVEFileArray$new(x)

    },

    #' @description Constructor
    #' @param x file array or can be converted to \code{\link[filearray]{as_filearray}}
    #' @param temporary whether this file array is to be deleted once the object
    #' is out-of-scope; default is false
    initialize = function(x, temporary = FALSE) {
      self$temporary <- temporary
      private$impl <- filearray::as_filearray(x)
    }

  ),
  active = list(

    #' @field valid whether the array is valid and ready to read
    valid = function() {
      if(!inherits(private$impl, "FileArray")) { return(FALSE) }
      private$impl$valid()
    },

    #' @field @impl the underlying array object
    `@impl` = function() {
      private$impl
    }
  )
)


#' @export
`[.RAVEFileArray` <- function(x, ...) {
  x$`@impl`[...]
}

#' @export
`[<-.RAVEFileArray` <- function(x, ..., value) {
  x$`@impl`[...] <- value
  x
}


