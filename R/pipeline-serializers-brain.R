tfmtreg_rave_brain <- function() {
  target_format_register(
    "rave-brain",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      indata <- readRDS(path)
      if (!is.list(indata) || !isTRUE(indata$class %in% c("rave-brain", "multi-rave-brain")) ||
         length(indata$params) == 0) {
        return(NULL)
      }

      # marshaling lives in R/serialize-brain.R, shared with the refhook
      # serializers (`rave_serialize_impl`) used by the default target format
      ravepipeline <- asNamespace("ravepipeline")

      if (identical(indata$class, "rave-brain")) {
        return(ravepipeline$brain_unmarshal(indata$params))
      }
      ravepipeline$multibrain_unmarshal(indata$params)
    },
    write = function(object, path, target_export = NULL) {

      ravepipeline <- asNamespace("ravepipeline")

      if (inherits(object, "rave-brain")) {
        cls <- "rave-brain"
        params <- ravepipeline$brain_marshal(object)
      } else if (inherits(object, "multi-rave-brain")) {
        cls <- "multi-rave-brain"
        params <- ravepipeline$multibrain_marshal(object)
      } else {
        warning("To save/load as `rave-brain`, the object class must be either `rave-brain` or `multi-rave-brain`")
        saveRDS(object = NULL, file = path, version = 3L)
        return(invisible())
      }

      saveRDS(object = list(class = cls, params = params), file = path, version = 3L)

    }
  )
}
