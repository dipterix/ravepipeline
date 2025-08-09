target_format_default_write <- function(object, path) {
  saveRDS(object = object, refhook = rave_serialize_refhook, file = path)
}

target_format_default_read <- function(path) {
  readRDS(path, refhook = rave_unserialize_refhook)
}

target_format_default_marshal <- function(object) {
  serialize(object, NULL, refhook = rave_serialize_refhook)
}

target_format_default_unmarshal <- function(object) {
  unserialize(object, refhook = rave_unserialize_refhook)
}

target_format_default <- function() {

  targets::tar_format(
    read = function(path) {
      asNamespace("ravepipeline")$target_format_default_read(path)
    },
    write = function(object, path) {
      asNamespace("ravepipeline")$target_format_default_write(object, path)
    },
    marshal = function(object) {
      asNamespace("ravepipeline")$target_format_default_marshal(object)
    },
    unmarshal = function(object) {
      asNamespace("ravepipeline")$target_format_default_unmarshal(object)
    }
  )

}


