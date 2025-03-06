load_yaml <- function(file, ..., map = NULL){
  re <- ieegio::io_read_yaml(con = file, ...)
  if(!inherits(map, 'fastmap2')){
    map <- dipsaus::fastmap2()
  }
  for(nm in names(re)){
    if(nm != ''){
      map[[nm]] <- re[[nm]]
    }
  }
  map
}

read_yaml <- function(file, ...) {
  ieegio::io_read_yaml(con = file, ...)
}

save_yaml <- function(x, file, ..., sorted = FALSE){

  ieegio::io_write_yaml(x = x, con = file, ..., sorted = sorted)

}


