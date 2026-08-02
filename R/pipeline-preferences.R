#' @title Pipeline preference management (low-level)
#' @name rave-pipeline-preferences
#' @description
#' Get, set, and check persistent preference values for 'RAVE' pipelines and
#' modules. Preferences are stored in a global on-disk store that survives
#' across R sessions.
#'
#' @details
#' Preference keys must follow a three-part dot-separated naming convention
#' \code{[prefix].[type].[key]}:
#' \describe{
#'   \item{\code{prefix}}{Either \code{"global"} (shared across all modules) or
#'     a specific module ID such as \code{"power_explorer"}.  When calling
#'     \code{pipeline_set_preferences} from within a pipeline, the allowed
#'     prefixes are automatically restricted to \code{"global"} and the
#'     current pipeline name.}
#'   \item{\code{type}}{A category string such as \code{"graphics"} or
#'     \code{"export"}.}
#'   \item{\code{key}}{The individual preference item, e.g.
#'     \code{"use_ggplot"} or \code{"default_format"}.}
#' }
#' Valid examples: \code{"global.graphics.use_ggplot"},
#' \code{"power_explorer.export.default_format"}.
#'
#' Setting a preference value to \code{NULL} removes the key from the store.
#'
#' These functions are low-level: every call repeats the full key, the default,
#' and the type constraint. Module code should prefer the declarative methods
#' \code{define_preference}, \code{use_preference}, and
#' \code{reset_preference} on the \code{\link{PipelineTools}} class, which
#' declare a preference once and then refer to it by a short name.
#'
#' @param keys one or more preference key strings following the
#'   \code{[prefix].[type].[key]} naming convention
#' @param ...,.list for \code{pipeline_set_preferences}: named values to store,
#'   where each name is a preference key; for \code{pipeline_get_preferences}:
#'   additional arguments forwarded to \code{validator}
#' @param .pipe_dir the active pipeline directory used to determine the allowed
#'   key prefix; defaults to the \code{RAVE_PIPELINE} environment variable or
#'   the current working directory
#' @param .preference_instance pipeline preference instance: this is
#'   automatically filled when calling from \code{pipeline$get_preferences()}
#'   When \code{NULL}, the shared on-disk preference store is used automatically
#' @param simplify if \code{TRUE} (default) and exactly one key is requested,
#'   return the value directly instead of a length-one named list
#' @param ifnotfound value returned when a requested key is absent or fails
#'   validation; default is \code{NULL}
#' @param validator \code{NULL} or a single-argument function that validates
#'   each retrieved value; any extra arguments in \code{...} are forwarded to
#'   it. If the function signals an error, \code{ifnotfound} is returned for
#'   that key instead
#' @param modes \code{NULL}, or a character vector of expected R
#'   \code{\link{mode}} strings (e.g. \code{"numeric"}, \code{"character"})
#'   recycled to match the length of \code{keys}. A stored value whose mode
#'   does not match is treated as missing and replaced by \code{ifnotfound}
#' @returns
#' \describe{
#'   \item{\code{pipeline_set_preferences}}{Invisibly returns the named list of
#'     values that were passed in.}
#'   \item{\code{pipeline_get_preferences}}{The preference value(s): a single
#'     value when \code{simplify = TRUE} and one key is requested, otherwise
#'     a named list with one element per key.}
#'   \item{\code{pipeline_has_preferences}}{A logical vector the same length as
#'     \code{keys} indicating which keys currently exist in the preference
#'     store.}
#' }
#' @examples
#' \dontrun{
#' # Set preferences (keys use [prefix].[type].[key] convention)
#' pipeline_set_preferences(
#'   "global.graphics.use_ggplot" = TRUE,
#'   "global.graphics.cex" = 1.2
#' )
#'
#' # Check whether keys exist
#' pipeline_has_preferences(
#'   c("global.graphics.use_ggplot", "global.graphics.cex")
#' )
#'
#' # Retrieve a single preference (returns the value directly)
#' pipeline_get_preferences("global.graphics.cex")
#'
#' # Retrieve multiple preferences as a named list
#' pipeline_get_preferences(
#'   keys = c("global.graphics.use_ggplot", "global.graphics.cex"),
#'   simplify = FALSE
#' )
#'
#' # Return a default when the key is absent
#' pipeline_get_preferences("global.graphics.missing_key", ifnotfound = FALSE)
#'
#' # Validate the stored mode; fall back to default on mismatch
#' pipeline_get_preferences(
#'   "global.graphics.cex",
#'   modes = "numeric",
#'   ifnotfound = 1.0
#' )
#'
#' # Remove a preference by setting it to NULL
#' pipeline_set_preferences("global.graphics.cex" = NULL)
#' }
#' @seealso \code{\link{PipelineTools}} for the declarative preference methods
#' \code{define_preference}, \code{use_preference}, and
#' \code{reset_preference}
#' @export
pipeline_set_preferences <- function(
    ..., .list = NULL,
    .pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
    .preference_instance = NULL) {
  prefs <- c(list(...), .list)
  if (!length(prefs)) { return(invisible()) }
  # preferences must be `global/module_id`.`type (graphics, ...)`.`key`.dtype
  nms <- names(prefs)
  if (length(nms) != length(prefs) || any(nms == "")) {
    stop("All preferences must be named")
  }

  if (missing(.preference_instance) || is.null(.preference_instance)) {
    pipe_dir <- activate_pipeline(.pipe_dir)
    pipeline_name <- attr(pipe_dir, "target_name")
    instance <- global_preferences(.prefix_whitelist = c("global", pipeline_name))
  } else {
    instance <- .preference_instance
  }

  instance$mset(.list = prefs)

  invisible(prefs)

}

#' @rdname rave-pipeline-preferences
#' @export
pipeline_get_preferences <- function(
    keys, simplify = TRUE, ifnotfound = NULL, validator = NULL, modes = NULL, ...,
    .preference_instance = NULL) {

  if (missing(.preference_instance) || is.null(.preference_instance)) {
    instance <- global_preferences()
  } else {
    instance <- .preference_instance
  }

  if (length(modes) > 0 && length(keys) > 0) {
    modes <- rep(modes, ceiling(length(keys) / length(modes)))
    modes <- structure(
      names = keys,
      as.list(modes[seq_along(keys)])
    )
  } else {
    modes <- NULL
  }

  if (is.function(validator) || length(modes) > 0) {
    args <- list(...)
    force(ifnotfound)

    validator_ <- function(key, value) {
      if (length(modes) > 0) {
        vm <- unlist(modes[[key]])
        vm <- vm[!is.na(vm) & vm != ""]
        if (length(vm) > 0) {
          if (any(c("integer", "double", "numeric") %in% vm)) {
            vm <- c(vm, c("integer", "double", "numeric"))
          }
          vm <- unique(vm)
          if (!any(mode(value) %in% vm)) {
            stop(sprintf("Value for `%s` is none of the following: %s", key, paste(vm, collapse = ", ")))
          }
        }
      }
      if (is.function(validator)) {
        do.call(validator, c(list(value), args))
      }
      return()
    }

    re <- structure(
      names = keys,
      lapply(keys, function(key) {
        if (instance$has(key)) {
          value <- instance$get(key, missing_default = ifnotfound)
          tryCatch({
            validator_(key = key, value = value)
            return(value)
          }, error = function(e) {
            ifnotfound
          })
        } else {
          ifnotfound
        }
      })
    )
  } else {
    re <- instance$mget(keys, missing_default = ifnotfound)
  }
  if (simplify && length(keys) == 1) {
    re <- re[[1]]
  }
  return(re)
}

#' @rdname rave-pipeline-preferences
#' @export
pipeline_has_preferences <- function(keys, ..., .preference_instance = NULL) {
  if (missing(.preference_instance) || is.null(.preference_instance)) {
    instance <- global_preferences()
  } else {
    instance <- .preference_instance
  }
  instance$has(keys, ...)
}

# ---- Declarative preferences (high-level) -----------------------------------
# These back `pipeline$define_preference()`, `pipeline$use_preference()`, and
# `pipeline$reset_preference()`. Unlike the low-level `pipeline_*_preferences`
# functions above, a preference is declared once (name, domain, type, default,
# validator) and afterwards referred to by its short name.
#
# Defaults are lazy: declaring a preference stores metadata only. The declared
# default materializes when reading a key that is absent, so the preference
# store contains exactly the values a user has explicitly changed, and revising
# a declared default takes effect immediately for anyone who never overrode it.

PREFERENCE_DOMAINS <- c("default", "graphics", "analysis", "export")

# Storage types (`typeof`), plus two special cases handled in
# `check_preference_type`: `numeric` (double or integer) and `named_list`
PREFERENCE_TYPES <- c(
  "any", "numeric", "named_list",
  "logical", "integer", "double", "character", "complex", "raw",
  "list", "closure", "environment", "language", "NULL"
)

# Returns `TRUE` when `value` matches `type`, otherwise a diagnostic string
check_preference_type <- function(value, type) {
  if (identical(type, "any")) { return(TRUE) }

  actual <- typeof(value)

  if (identical(type, "numeric")) {
    if (actual %in% c("double", "integer")) { return(TRUE) }
    return(sprintf("expecting `numeric` (`double` or `integer`) value, but the value is `%s`", actual))
  }

  if (identical(type, "named_list")) {
    if (!identical(actual, "list")) {
      return(sprintf("expecting `named_list` value, but the value is `%s`", actual))
    }
    if (!length(value)) { return(TRUE) }
    nms <- names(value)
    if (length(nms) == length(value) && !anyNA(nms) && all(nms != "")) { return(TRUE) }
    return("expecting `named_list` value: the value is a list, but not all the elements are named")
  }

  if (identical(actual, type)) { return(TRUE) }
  sprintf("expecting `%s` value, but the value is `%s`", type, actual)
}

# Metadata is stored in the same preference store, under its own `type`
# component, so it inherits the prefix white-list. The domain is part of the
# key: the same name may be declared in more than one domain.
preference_metadata_key <- function(namespace, domain, name) {
  sprintf("%s.preference_metadata.%s.%s", namespace, domain, name)
}

# Rebuilds a stand-alone validator from `metadata`. The validator is stored as
# deparsed source rather than as a closure so it stays portable across sessions
# and machines; it is therefore evaluated in a fresh environment whose parent is
# this namespace, and it cannot see variables from wherever it was written.
construct_preference_validator <- function(metadata) {
  new_function2(
    args = alist(value = , pipeline = ),
    env = new.env(parent = asNamespace("ravepipeline")),
    quote_type = "quote",
    body = bquote({
      # the missing-key sentinel is a bare list, and would otherwise satisfy
      # the `list` / `named_list` / `any` checks
      if (is_key_missing(value)) {
        stop(sprintf("Preference \"%s\" has no value.", .(metadata$name)))
      }

      .type_check <- check_preference_type(value, .(metadata$type))
      if (!isTRUE(.type_check)) {
        stop(sprintf("Preference \"%s\": %s.", .(metadata$name), .type_check))
      }

      .validator_str <- .(metadata$validator)
      if (length(.validator_str) > 0) {
        .validator_expr <- parse(text = .validator_str)
        .validator_env <- environment()
        # suppress print messages and only emit messages/warnings
        utils::capture.output({
          vres <- eval(.validator_expr, envir = .validator_env)
        }, type = "output")

        if (is.character(vres)) {
          stop(vres)
        }
        if (identical(vres, FALSE)) {
          stop(sprintf("Preference \"%s\" did not pass its validator.", .(metadata$name)))
        }
        if (!isTRUE(vres) && !is.null(vres)) {
          stop(sprintf("Validator for `%s` did not return TRUE/FALSE/NULL nor error string. The preference validator is malformed",
                       .(metadata$name)))
        }
      }
      invisible(TRUE)
    })
  )
}

# Resolves `name` (either a bare key name, or a full `namespace.domain.name`
# key) to its metadata; returns `KEY_MISSING` when the preference was never
# declared.
get_preference_metadata <- function(pipeline, name) {
  force(pipeline)

  name <- tolower(trimws(name))
  if (length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("Invalid preference name: must be a length-one, non-blank string.")
  }

  if (grepl(".", name, fixed = TRUE)) {
    parsed <- strsplit(name, ".", fixed = TRUE)[[1]]
    if (length(parsed) != 3) {
      stop("Invalid preference name: name must be either 'namespace.domain.key_name' style or just simple key name without any dot.")
    }
    namespaces <- parsed[[1]]
    domains <- parsed[[2]]
    name <- parsed[[3]]

    if (!domains %in% PREFERENCE_DOMAINS) {
      stop(sprintf(
        "Invalid preference domain `%s`. Choices are: %s",
        domains, paste(sprintf("`%s`", PREFERENCE_DOMAINS), collapse = ", ")
      ))
    }
  } else {
    # module-level declarations shadow the global ones
    namespaces <- unique(c(pipeline$pipeline_name, "global"))
    domains <- PREFERENCE_DOMAINS
  }

  for (namespace in namespaces) {
    for (domain in domains) {
      metakey <- preference_metadata_key(namespace, domain, name)
      if (pipeline$has_preferences(metakey)) {
        metadata <- pipeline$get_preferences(metakey, ifnotfound = KEY_MISSING)
        if (inherits(metadata, "ravepipeline_preference_metadata")) {
          return(metadata)
        }
      }
    }
  }

  return(KEY_MISSING)
}

define_preference <- function(
    pipeline, name, default = NULL,
    type = PREFERENCE_TYPES,
    validator = NULL,
    domain = PREFERENCE_DOMAINS, global = FALSE,
    verbose = TRUE) {

  force(pipeline)
  domain <- match.arg(domain)
  type <- match.arg(type)
  global <- as.logical(global)[[1]]

  name <- tolower(name)
  stopifnot(
    "Preference name length must be 1" = length(name) == 1,
    "Preference name may only contain letters, underscores, dashes, digits" =
      isTRUE(grepl("^[a-z0-9_-]{1,}$", name))
  )

  if (global) {
    pref_key_ns <- "global"
  } else {
    pref_key_ns <- pipeline$pipeline_name
  }

  pref_key <- sprintf("%s.%s.%s", pref_key_ns, domain, name)
  pref_metakey <- preference_metadata_key(pref_key_ns, domain, name)

  if (is.function(validator)) {
    fmls <- formals(validator)
    stopifnot(
      "`validator` must be a function (if not `NULL`) containing formals no more than `value`, `pipeline`" =
        all(names(fmls) %in% c("value", "pipeline"))
    )
    validator <- deparse(body(validator))
  } else {
    stopifnot(
      "`validator` must be a function or `NULL`" = is.null(validator)
    )
  }

  metadata <- list(
    name = name,
    global = global,
    key = pref_key,
    default = default,
    type = type,
    validator = validator
  )

  metadata_signature <- digest(metadata)
  metadata$signature <- metadata_signature
  class(metadata) <- "ravepipeline_preference_metadata"

  validator_reconstructed <- construct_preference_validator(metadata)

  # `default` is allowed to be `NULL`: that declares a preference that simply
  # has no value until one is set
  if (!is.null(default)) {
    pass <- tryCatch({
      validator_reconstructed(value = default, pipeline = pipeline)
    }, error = function(e) {
      e
    })

    if (!isTRUE(pass)) {
      stop("The default value does not pass the validator. The validator returned: \n", pass$message)
    }
  }

  metadata_exists <- FALSE
  if (pipeline$has_preferences(pref_metakey)) {
    metadata_old <- pipeline$get_preferences(pref_metakey)
    if (inherits(metadata_old, "ravepipeline_preference_metadata")) {
      if (isTRUE(metadata_old$signature == metadata$signature)) {
        metadata_exists <- TRUE
      } else if (!isTRUE(metadata_old$key == pref_key)) {
        # the declaration moved namespace or domain: drop the orphaned value
        if (verbose) {
          logger(
            "Preference `{metadata_old$key}` has been re-declared as `{pref_key}`: removing the stale value.",
            level = "trace",
            use_glue = TRUE
          )
        }
        pipeline$set_preferences(.list = structure(
          names = metadata_old$key,
          list(NULL)
        ))
      }
    }
  }

  if (!metadata_exists) {
    pipeline$set_preferences(.list = structure(
      names = pref_metakey,
      list(metadata)
    ))
  }

  invisible(metadata)
}

use_preference <- function(pipeline, name, value, verbose = TRUE) {

  force(pipeline)

  metadata <- get_preference_metadata(pipeline = pipeline, name = name)

  if (is_key_missing(metadata)) {
    stop(sprintf(
      "Preference metadata for name `%s` unset. Please use `pipeline$define_preference(...)` first.",
      name
    ))
  }

  validator_reconstructed <- construct_preference_validator(metadata)

  if (!missing(value)) {
    # setting `NULL` removes the key, which is how a preference is reset back
    # to its declared default
    if (!is.null(value)) {
      validator_reconstructed(value = value, pipeline = pipeline)
    }

    pipeline$set_preferences(.list = structure(
      names = metadata$key,
      list(value)
    ))
  }

  # Reads never write: an unset key, or one whose stored value no longer passes
  # the validator (declaration changed, or the low-level setter was used),
  # simply reads as the declared default.
  current <- pipeline$get_preferences(
    keys = metadata$key,
    simplify = TRUE,
    ifnotfound = KEY_MISSING
  )

  if (is_key_missing(current)) {
    return(metadata$default)
  }

  invalid <- tryCatch({
    validator_reconstructed(value = current, pipeline = pipeline)
    NULL
  }, error = function(e) {
    e
  })

  if (is.null(invalid)) {
    return(current)
  }

  if (verbose) {
    logger(
      "Preference `{metadata$key}` has an invalid stored value ({invalid$message}), using the declared default instead.",
      level = "trace",
      use_glue = TRUE
    )
  }

  metadata$default

}

reset_preference <- function(pipeline, name, verbose = TRUE) {

  force(pipeline)

  metadata <- get_preference_metadata(pipeline = pipeline, name = name)
  if (is_key_missing(metadata)) {
    return(invisible(FALSE))
  }

  if (verbose) {
    logger(
      "Resetting preference `{metadata$key}` to its declared default.",
      level = "trace",
      use_glue = TRUE
    )
  }

  # removing the key is enough: reads fall back to the declared default
  pipeline$set_preferences(.list = structure(
    names = metadata$key,
    list(NULL)
  ))

  invisible(TRUE)
}
