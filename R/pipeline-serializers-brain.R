tfmtreg_rave_brain <- function() {
  target_format_register(
    "rave-brain",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      indata <- readRDS(path)
      if(!is.list(indata) || !isTRUE(indata$class %in% c("rave-brain", "multi-rave-brain")) ||
         length(indata$params) == 0) {
        return(NULL)
      }

      # 3D brain is defined in raveio
      ravepipeline <- asNamespace('ravepipeline')

      restore_brain <- function(params) {
        if(inherits(params, "rave-brain")) {
          return(params)
        }
        if(!length(params)) { return(NULL) }
        tryCatch({

          RAVESubject <- ravepipeline$call_ravecore_fun(f_name = "RAVESubject", .call_pkg_function = FALSE)
          rave_brain <- ravepipeline$call_ravecore_fun(f_name = "rave_brain", .call_pkg_function = FALSE)

          subject <- RAVESubject$new(
            project_name = params$project_name,
            subject_code = params$subject_code,
            strict = FALSE
          )
          brain <- tryCatch({
            rave_brain(
              subject = subject,
              surfaces = params$surfaces,
              use_141 = params$use_141,
              usetemplateifmissing = params$usetemplateifmissing,
              include_electrodes = FALSE
            )
          }, error = function(e) {
            rave_brain(
              subject = subject,
              use_141 = params$use_141,
              usetemplateifmissing = params$usetemplateifmissing,
              include_electrodes = FALSE
            )
          })
          if(!inherits(brain, c("rave-brain", "multi-rave-brain"))) {
            warning(sprintf("Cannot import 3D model - [%s]", subject$subject_id))
            return(NULL)
          }
          if(is.data.frame(params$electrode_table)) {
            brain$set_electrodes(params$electrode_table)

            if(is.data.frame(params$electrode_values)) {
              brain$set_electrode_values(params$electrode_values)
            }
          }
          return(brain)
        }, error = function(e) {
          e$message <- sprintf("Cannot import 3D model - [%s]. Reason: %s",
                               paste(params$subject_code, collapse = ""),
                               paste(e$message, collapse = ""))
          warning(e)
          return(NULL)
        })
      }
      if(indata$class == "rave-brain") {
        return(restore_brain(indata$params))
      } else {
        # restore template
        template_params <- indata$params$template_params
        individual_params <- indata$params$individual_params

        # load subjects' brain
        blist <- lapply(individual_params, restore_brain)
        blist <- blist[!vapply(blist, is.null, FUN.VALUE = logical(1))]
        if(!is.list(blist)) {
          blist <- as.list(blist)
        }

        threeBrain <- asNamespace("threeBrain")
        brain <- tryCatch({
          threeBrain$merge_brain(
            .list = blist,
            template_surface_types = template_params$surfaces,
            template_subject = template_params$subject_code
          )
        }, error = function(e) {
          threeBrain$merge_brain(.list = blist)
        })

        etable <- template_params$electrode_table
        if(length(brain$template_object)) {
          if(is.data.frame(etable) && nrow(etable)) {
            if(length(etable$Subject)) {
              etable$Subject[etable$Subject == template_params$subject_code] <- brain$template_object$subject_code
            }
            brain$template_object$set_electrodes(etable)

            vtable <- template_params$electrode_values
            if(is.data.frame(vtable) && nrow(vtable)) {
              if(length(vtable$Subject)) {
                vtable$Subject[vtable$Subject == template_params$subject_code] <- brain$template_object$subject_code
              }
              brain$template_object$set_electrode_values(vtable)
            }
          }
        }


        brain
      }
    },
    write = function(object, path, target_export = NULL) {

      if(!inherits(object, c("rave-brain", "multi-rave-brain"))) {
        warning("To save/load as `rave-brain`, the object class must be either `rave-brain` or `multi-rave-brain`")
        saveRDS(object = NULL, file = path, version = 3L)
      }
      get_constructor <- function(brain) {
        if(!inherits(brain, c("rave-brain", "multi-rave-brain"))) {
          return(NULL)
        }
        # check construction
        params <- brain$meta$constructor_params
        if(all(c("project_name", "subject_code") %in% names(params)) &&
           length(params$project_name) == 1 &&
           length(params$subject_code) == 1) {
          params$use_141 <- isTRUE(as.logical(params$use_141))
          params$usetemplateifmissing <- isTRUE(as.logical(params$usetemplateifmissing))
          params <- params[c("project_name", "subject_code", "use_141", "usetemplateifmissing")]
          params$surfaces <- names(brain$surfaces)
          params$electrode_table <- brain$electrodes$raw_table
          params$electrode_values <- brain$electrodes$value_table
          return(params)
        } else {
          return(brain)
        }
      }

      cls <- NULL
      if(inherits(object, "rave-brain")) {
        cls <- "rave-brain"
        params <- get_constructor(object)
      } else {
        cls <- "multi-rave-brain"

        individual_params <- lapply(object$objects, get_constructor)
        individual_params[!vapply(individual_params, is.null, FUN.VALUE = logical(1))]

        params <- list(
          template_params = list(
            subject_code = object$template_subject,
            electrode_table = object$template_object$electrodes$raw_table,
            electrode_values = object$template_object$electrodes$value_table,
            surfaces = object$surface_types
          ),
          individual_params = individual_params
        )
      }

      saveRDS(object = list(class = cls, params = params), file = path, version = 3L)

    }
  )
}
