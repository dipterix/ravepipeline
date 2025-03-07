# This file is licensed under MIT by Zhengjia Wang

load_yaml <- function(file, ..., map = NULL){
  re <- yaml::read_yaml(file = file, ...)
  if(!inherits(map, 'fastmap2')){
    map <- fastmap2()
  }
  for(nm in names(re)){
    if(nm != ''){
      map[[nm]] <- re[[nm]]
    }
  }
  map
}

read_yaml <- function(file, ...) {
  yaml::read_yaml(file = file, ...)
}

save_yaml <- function(x, file, ..., sorted = FALSE) {
  if (inherits(x, "fastmap")) {
    x <- x$as_list(sort = sorted)
  }
  else if (inherits(x, "fastmap2")) {
    x <- x[["@as_list"]](sort = sorted)
  }
  else if (inherits(x, c("fastqueue", "fastqueue2"))) {
    x <- x$as_list()
  }
  else if (sorted) {
    x <- as.list(x, sorted = sorted, ...)
  }
  else {
    x <- as.list(x, ...)
  }
  yaml::write_yaml(x, file = file, ...)
  if (!inherits(file, "connection")) {
    file <- normalizePath(file)
  }
  invisible(file)
}


