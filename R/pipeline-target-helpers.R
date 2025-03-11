get_target_deps <- function(target) {
  if("deps" %in% names(target$command)) {
    return(target$command$deps)
  }
  return(target$deps)
}


get_target_name <- function(target) {
  target$settings$name
}

get_target_expr <- function(target) {
  target$command$expr
}
