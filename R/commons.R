# This file is licensed under MIT by Zhengjia Wang

`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}


append_el <- function(el, value, method = 'c'){
  el_expr <- substitute(el)
  value <- do.call(method, list(quote(el), quote(value)))
  do.call('<-', list(el_expr, value), envir = parent.frame())
}

package_installed <- function (pkgs, all = FALSE) {
  re <- sapply(pkgs, function(p) {
    system.file("", package = p) != ""
  })
  if (all) {
    re <- all(re)
  }
  re
}

require_package <- function(package, return_namespace = FALSE) {
  # if(system.file(package = package) == "") {
  #   stop(sprintf("Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')", package, package), call. = NULL)
  #
  # }
  targets::tar_assert_package(package)
  if( return_namespace ) {
    return(asNamespace(package))
  }
  return(invisible())
}

call_pkg_fun <- function(package, f_name, ...,
                         .if_missing = c("error", "warning", "none"),
                         .missing_default = NULL,
                         .call_pkg_function = TRUE) {

  stopifnot(length(package) == 1)

  if(!package_installed(package)) {
    .if_missing <- match.arg(.if_missing)
    switch(
      .if_missing,
      "error" = {
        stop("Package ", sQuote(package), " is missing.")
      },
      "warning" = {
        warning("Package ", sQuote(package), " is missing.")
      },
      {}
    )
    return(.missing_default)
  }

  ns <- asNamespace(package)
  fun <- ns[[f_name]]

  if( .call_pkg_function ) {
    if(!is.function(fun)) {
      .if_missing <- match.arg(.if_missing)
      switch(
        .if_missing,
        "error" = {
          stop("Package ", sQuote(package), " does not have function ", sQuote(f_name))
        },
        "warning" = {
          warning("Package ", sQuote(package), " does not have function ", sQuote(f_name))
        },
        {}
      )
      return(.missing_default)
    }

    return(fun(...))
  } else {
    return(fun)
  }

}

safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE){
  suppressWarnings({
    if(get_os() == 'windows'){
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA){

  if(Sys.which(cmd) == ""){
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if(is.function(onFound)){
    ret <- onFound(ret)
  }
  ret
}


new_function2 <- function (
    args = alist(), body = {},
    env = parent.frame(),
    quote_type = c("unquoted", "quote", "quo"),
    quasi_env = parent.frame()) {

  quote_type <- match.arg(quote_type)
  switch(quote_type, unquoted = {
    quo <- eval(as.call(list(quote(rlang::quo), substitute(body))),
                envir = quasi_env)
    body <- rlang::quo_squash(quo)
  }, quote = {
    quo <- eval(as.call(list(quote(rlang::quo), body)), envir = quasi_env)
    body <- rlang::quo_squash(quo)
  }, quo = {
    body <- rlang::quo_squash(quo)
  })
  f <- local({
    function() {
    }
  }, envir = env)
  formals(f) <- args
  body(f) <- body
  f

}
