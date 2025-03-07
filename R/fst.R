# This file is licensed under MIT by Zhengjia Wang

io_read_fst <- function (con, method = c("proxy", "data_table", "data_frame",
                                         "header_only"), ..., old_format = FALSE) {
  method <- match.arg(method)
  switch(method, proxy = {
    fst::fst(path = con, old_format = old_format)
  }, data_table = {
    fst::read_fst(path = con, as.data.table = TRUE, old_format = old_format,
                  ...)
  }, data_frame = {
    fst::read_fst(path = con, as.data.table = FALSE, old_format = old_format,
                  ...)
  }, header_only = {
    fst::metadata_fst(path = con, old_format = old_format)
  })
}


io_write_fst <- function (x, con, compress = 50, ...) {
  fst::write_fst(x = x, path = con, compress = compress, ...)
}

load_fst <- function(path, ..., as.data.table = TRUE){
  if( !is.character(as.data.table) ) {
    if( as.data.table ) {
      method <- "data_table"
    } else {
      method <- "data_frame"
    }
  } else {
    method <- as.data.table
  }
  tryCatch({
    io_read_fst(con = path, method = method, ...)
    # fst::read_fst(path, ..., as.data.table = as.data.table)
  }, error = function(e){
    stop("FST load failure: ", path)
  })
}

save_fst <- function(x, path, ...){
  # catgl('Writing to path: {path}')
  dir <- dirname(path)
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  io_write_fst(x = x, con = path, ...)
  # fst::write_fst(x = x, path = path, ...)
}

