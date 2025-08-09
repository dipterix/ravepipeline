#' @title Abstract class for 'RAVE' serialization
#' @description
#' For package inheritance only; do not instantiate the class directly.
#' @seealso \code{\link{RAVEFileArray}} \code{\link{rave-serialize-refhook}}
#' @export
RAVESerializable <- R6::R6Class(
  classname = "RAVESerializable",
  portable = TRUE,
  public = list(
    #' @description Abstract constructor
    #' @param ... ignored
    initialize = function(...) {
      stop("RAVESerializable$new should not be called directory. Implement a child class instead.")
    },

    #' @description Create an atomic list that can be serialized
    #' @param ... ignored
    `@marshal` = function(...) {
      .NotYetImplemented()
    },

    #' @description Restore an object from an atomic list
    #' @param object a list from \code{'@marshal'}
    #' @param ... ignored
    `@unmarshal` = function(object, ...) {
      .NotYetImplemented()
    },

    #' @description How two object can be compared to each other
    #' @param other another object to compare with self
    `@compare` = function(other) {
      if(!R6::is.R6(other)) { return(FALSE) }
      if(!identical(class(self), class(other))) { return(FALSE) }
      tryCatch(
        {
          identical(self$`@marshal`(), other$`@marshal`())
        },
        error = function(e) {
          FALSE
        }
      )
    }
  )
)

#' @export
`==.RAVESerializable` <- function(e1, e2) {
  e1$`@compare`(e2)
}

#' @export
`!=.RAVESerializable` <- function(e1, e2) {
  !e1$`@compare`(e2)
}

#' @name rave-serialize-refhook
#' @title Serialization reference hook generic functions
#' @param object Object to serialize (environment or external pointers)
#' @param x raw or string objects that will be passed to
#' \code{\link{unserialize}} function before further reconstruction
#' @returns \code{rave_serialize_refhook} returns either serialized objects
#' in string (raw vector converted to char via \code{rawToChar}), or \code{NULL}
#' indicating the object undergoing default serialization;
#' \code{rave_unserialize_refhook} returns the reconstructed object.
#' @examples
#'
#' # This example requires additional `filearray` package
#' # If you are an RAVE user (installed RAVE via rave.wiki)
#' # then this package was installed
#'
#' x0 <- array(rnorm(240000), c(200, 300, 4))
#' x1 <- filearray::as_filearray(x0)
#' x2 <- RAVEFileArray$new(x1, temporary = TRUE)
#'
#' r0 <- serialize(x0, NULL, refhook = rave_serialize_refhook)
#' r1 <- serialize(x1, NULL, refhook = rave_serialize_refhook)
#' r2 <- serialize(x2, NULL, refhook = rave_serialize_refhook)
#'
#' # Compare the serialization sizes
#' c(length(r0), length(r1), length(r2))
#'
#' y0 <- unserialize(r0, refhook = rave_unserialize_refhook)
#' y1 <- unserialize(r1, refhook = rave_unserialize_refhook)
#' y2 <- unserialize(r2, refhook = rave_unserialize_refhook)
#'
#' all(y0 == x0)
#' all(y1[] == x0)
#' all(y2[] == x0)
#'
#' \dontrun{
#'
#' # 3D Brain, this example needs RAVE installation, not included in
#' # this package, needs extra installations available at rave.wiki
#'
#' # 4 MB
#' brain <- ravecore::rave_brain("demo/DemoSubject")
#'
#' # 52 KB
#' rbrain <- serialize(brain, NULL, refhook = rave_serialize_refhook)
#'
#' brain2 <- unserialize(rbrain, refhook = rave_unserialize_refhook)
#'
#' brain2$plot()
#'
#' }
#'
#' @export
rave_serialize_refhook <- function(object) {
  rave_serialize_impl(object)
}

#' @rdname rave-serialize-refhook
#' @export
rave_serialize_impl <- function(object) {
  UseMethod("rave_serialize_impl")
}

#' @rdname rave-serialize-refhook
#' @export
rave_serialize_impl.default <- function(object) {
  NULL
}

#' @rdname rave-serialize-refhook
#' @export
rave_serialize_impl.RAVESerializable <- function(object) {
  if(R6::is.R6(object)) {
    # object$`@serialize_refhook`()
    x <- object$`@marshal`()

    if(!is.list(x) || length(x$namespace) != 1 || length(x$r6_generator) != 1) {
      stop("`RAVESerializable` object must be marshal'd to a list containing a namespace, an R6 generator, and a data list")
    }

    rawToChar(serialize(structure(x, class = c("rave_serialized_r6", "rave_serialized")), NULL, ascii = TRUE))
  } else {
    NextMethod()
  }
}

#' @rdname rave-serialize-refhook
#' @export
`rave_serialize_impl.rave-brain` <- function(object) {
  if(!R6::is.R6(object)) {
    return(NextMethod())
  }
  # "rave-brain" "R6"

  # check construction
  params <- object$meta$constructor_params

  if(all(c("project_name", "subject_code") %in% names(params)) &&
     length(params$project_name) == 1 &&
     length(params$subject_code) == 1) {
    params$use_141 <- isTRUE(as.logical(params$use_141))
    params$usetemplateifmissing <- isTRUE(as.logical(params$usetemplateifmissing))
    params <- params[c("project_name", "subject_code", "use_141", "usetemplateifmissing")]
  } else {
    # brain is not created with ravecore/raveio
    params$project_name <- NA_character_
    params$subject_code <- object$subject_code
    params$use_141 <- FALSE
    params$usetemplateifmissing <- FALSE
    params$base_path <- object$base_path
  }
  params$volume_types <- object$volume_types
  params$surface_types <- object$surface_types
  params$atlas_types <- object$atlas_types

  params$electrode_table <- object$electrodes$raw_table
  params$electrode_values <- object$electrodes$value_table
  params$native_subject <- TRUE
  class(params) <- c("rave_serialized_rave-brain", "rave_serialized")
  rawToChar(serialize(params, NULL, ascii = TRUE))
}

#' @rdname rave-serialize-refhook
#' @export
rave_unserialize_refhook <- function(x) {
  if(!is.raw(x)) {
    x <- charToRaw(x)
  }
  li <- unserialize(x)
  rave_unserialize_impl(li)
}

#' @rdname rave-serialize-refhook
#' @export
rave_unserialize_impl <- function(x) {
  UseMethod("rave_unserialize_impl")
}

#' @rdname rave-serialize-refhook
#' @export
rave_unserialize_impl.default <- function(x) {
  x
}

#' @rdname rave-serialize-refhook
#' @export
rave_unserialize_impl.rave_serialized <- function(x) {
  stop("Unrecognized serialization for RAVE class.")
}

#' @rdname rave-serialize-refhook
#' @export
rave_unserialize_impl.rave_serialized_r6 <- function(x) {
  ns <- asNamespace(x$namespace)
  cls <- ns[[x$r6_generator]]
  if(!R6::is.R6Class(cls)) {
    stop("Unable to unserialize object from class definition: ", x$namespace, "::", x$r6_generator)
  }
  return(cls$public_methods$`@unmarshal`(object = x))
}

#' @rdname rave-serialize-refhook
#' @export
`rave_unserialize_impl.rave_serialized_rave-brain` <- function(x) {

  threeBrain <- require_package("threeBrain", return_namespace = TRUE)

  params <- x

  project_name <- params$project_name
  if(length(project_name) != 1 || is.na(project_name)) {
    brain <- threeBrain$threeBrain(
      path = params$base_path,
      subject_code = params$subject_code,
      surface_types = params$surface_types,
      atlas_types = params$atlas_types
    )
  } else {
    brain <- call_ravecore_fun("rave_brain", sprintf("%s/%s", params$project_name, params$subject_code),
                               surfaces = params$surface_types,
                               overlays = params$atlas_types,
                               usetemplateifmissing = isTRUE(params$usetemplateifmissing),
                               include_electrodes = FALSE)
  }
  if(is.null(brain)) {
    stop("Unable to restore the brain object.")
  }
  if(length(params$electrode_table)) {
    brain$set_electrodes(params$electrode_table)
  }
  if(length(params$electrode_values)) {
    brain$set_electrode_values(params$electrode_values)
  }
  brain
}
