#' @title Declare a color palette preference
#' @description
#' Convenience wrappers around \code{pipeline$define_preference} that declare a
#' palette preference in the \code{"graphics"} domain, validate it against the
#' matching table in \code{\link{rave-colormaps}}, and resolve the stored
#' palette name to the colors themselves. Use
#' \code{define_preference_discrete_colormap} for categorical variables and
#' \code{define_preference_continuous_colormap} for numeric heat maps.
#'
#' Re-declaring is cheap and safe: the stored preference metadata is rewritten
#' only when the declaration actually changes, and a user's chosen palette is
#' never overwritten.
#' @param pipeline a \code{\link{PipelineTools}} instance
#' @param default palette name used whenever the preference is unset; must be a
#' valid name for the corresponding table
#' @param verbose whether to emit trace-level logs; default is true
#' @returns Invisibly, a list with the stored palette name
#' (\code{preference_value}), whether the declaration was rewritten
#' (\code{metadata_updated}), and the preference \code{metadata}. Read the
#' colors themselves with \code{pipeline$use_preference}, which runs the
#' declared \code{getter}
#' @examples
#'
#' library(ravepipeline)
#' if(interactive() && length(pipeline_list()) > 0) {
#'   pipeline <- pipeline("power_explorer")
#'
#'   res <- define_preference_discrete_colormap(pipeline)
#'   res$preference_value    # the palette name
#'
#'   # the colors, plus the name they came from
#'   colors <- pipeline$use_preference("discrete_colormap")
#'   attr(colors, "preference_value")
#'
#'   pipeline$use_preference("discrete_colormap", value = "tab10")
#' }
#'
#' @name define_preference_colormap
NULL

#' @rdname define_preference_colormap
#' @export
define_preference_discrete_colormap <- function(
  pipeline,
  default = "default",
  verbose = TRUE
) {
  define_preference(
    pipeline = pipeline,
    name = "discrete_colormap",
    default = default,
    domain = "graphics",
    type = "character",
    verbose = verbose,
    validator = function(value) {
      seeds <- DISCRETE_COLORMAPS(preview = FALSE)
      seed_names <- names(seeds)
      if (!isTRUE(value %in% seed_names)) {
        return(sprintf(
          "Invalid discrete palette name `%s`... Available choices are: %s",
          value,
          paste(seed_names, collapse = ", ")
        ))
      }
      return(TRUE)
    },
    getter = function(value) {
      DISCRETE_COLORMAPS(value, error_if_missing = FALSE, preview = FALSE)
    }
  )
}

#' @rdname define_preference_colormap
#' @export
define_preference_continuous_colormap <- function(
    pipeline,
    default = "default",
    verbose = TRUE
) {
  define_preference(
    pipeline = pipeline,
    name = "continuous_colormap",
    default = default,
    domain = "graphics",
    type = "character",
    verbose = verbose,
    validator = function(value) {
      seeds <- CONTINUOUS_COLORMAPS(preview = FALSE)
      seed_names <- names(seeds)
      if (!isTRUE(value %in% seed_names)) {
        return(sprintf(
          "Invalid continuous palette name `%s`... Available choices are: %s",
          value,
          paste(seed_names, collapse = ", ")
        ))
      }
      return(TRUE)
    },
    getter = function(value) {
      CONTINUOUS_COLORMAPS(value, error_if_missing = FALSE, preview = FALSE)
    }
  )
}
