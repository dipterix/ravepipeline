default_settings <- local({
  defaults <- list()

  ensure_defaults <- function(){
    if(!length(defaults)){
      defaults[['..temp']] <- list()
      defaults[['tensor_temp_path']] <- '~/rave_data/cache_dir/'
      defaults[['verbose_level']] <- 'DEBUG'
      defaults[['raw_data_dir']] <- '~/rave_data/raw_dir/'
      defaults[['data_dir']] <- '~/rave_data/data_dir/'
      defaults[['bids_data_dir']] <- '~/rave_data/bids_dir/'
      defaults[['file_structure']] <- 'native'

      # Not validated (but not recommended to change)
      defaults[['module_root_dir']] <- '~/rave_modules/'
      defaults[['module_lookup_file']] <- '~/rave_modules/modules.csv'
      defaults[['delay_input']] <- 20
      defaults[['test_mode']] <- FALSE
      defaults[['fast_cache']] <- TRUE
      defaults[['image_width']] <- 1280L
      defaults[['image_height']] <- 768L
      defaults[['drive_speed']] <- c(50, 20)
      defaults[['disable_startup_speed_check']] <- FALSE
      defaults[['max_worker']] <- parallel::detectCores() - 1
      defaults[['disable_fork_clusters']] <- FALSE
      ram <- tryCatch({
        get_ram() / 1024^3
      }, error = function(e){
        8
      })
      if(is.na(ram) || ram < 0.5){
        ram <- 8
      }
      defaults[['max_mem']] <- ram

      # Not used
      defaults[['server_time_zone']] <- 'America/Chicago'
      defaults[['suma_nodes_per_electrodes']] <- 42L
      defaults[['matlab_path']] <- '/Applications/MATLAB_R2016b.app/bin'
      defaults[['py2_path']] <- ''
      defaults[['py3_path']] <- ''
      defaults[['py_virtualenv']] <- ''
    }
    defaults <<- defaults
  }

  function(s = fastmap2()){
    ensure_defaults()
    list_to_fastmap2(defaults, map = s)
    s
  }
})

validate_settings <- function(s = fastmap2(), ns_check = TRUE){
  d <- default_settings()

  # ------------- Temporary tensor path --------------
  tpath <- s[['tensor_temp_path']]
  if(length(tpath) == 0){
    s[['tensor_temp_path']] <- d[['tensor_temp_path']]
  } else if(length(tpath) > 1 || !isTRUE(is.character(tpath))){
    warning('Option tensor_temp_path is not length 1 character, reset to default')
    s[['tensor_temp_path']] <- d[['tensor_temp_path']]
  }
  tpath <- s[['tensor_temp_path']]
  # Set options so that ravetools can use this path
  # options("ravetools.tempdir" = tpath)
  # Sys.setenv("RAVETOOLS_TEMPDIR" = tpath)

  # ------------- catgl verbose level --------------
  verbose <- s[['verbose_level']]
  verbose <- verbose[verbose %in% c('DEBUG', 'DEFAULT', 'INFO', 'WARNING', 'ERROR', 'FATAL')]
  if(length(verbose) == 0){
    warning('Option verbose_level is not valid. Choices are: ',
            '"DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", and "FATAL". ',
            'Reset to default.')
    verbose <- d[['verbose_level']]
  }
  s[['verbose_level']] <- verbose[[1]]

  # ------------- Raw data path --------------
  raw_dir <- s[['raw_data_dir']]
  raw_dir <- trimws(raw_dir)
  if(length(raw_dir) != 1 || !isTRUE(is.character(raw_dir)) || raw_dir %in% c('', '.', '/')){
    warning('raw_data_dir should be a length 1 character to root of the raw data directories')
    raw_dir <- d[['raw_data_dir']]
  }
  s[['raw_data_dir']] <- normalizePath(raw_dir, mustWork = FALSE)

  # ------------- RAVE data path --------------
  data_dir <- s[['data_dir']]
  if(length(data_dir) != 1 || !isTRUE(is.character(data_dir)) || data_dir %in% c('', '.', '/')){
    warning('data_dir should be a length 1 character to root of the rave data directories')
    data_dir <- d[['data_dir']]
  }
  s[['data_dir']] <- normalizePath(data_dir, mustWork = FALSE)

  # ------------- BIDS data path --------------
  bids_dir <- s[['bids_data_dir']]
  if(length(bids_dir) != 1 || !isTRUE(is.character(bids_dir)) || bids_dir %in% c('', '.', '/')){
    warning('bids_data_dir should be a length 1 character to root of the BIDS data directories')
    bids_dir <- d[['bids_data_dir']]
  }
  s[['bids_data_dir']] <- normalizePath(bids_dir, mustWork = FALSE)

  # ------------- File structure: BIDS/native --------------
  file_structure <- s[['file_structure']]
  if(length(file_structure) != 1 || !isTRUE(is.character(file_structure)) || !file_structure %in% c('native', 'BIDS')){
    warning('file_structure can only be ', sQuote('native'), ' or ', sQuote('BIDS'), '. Reseting to `native`')
    file_structure <- d[['file_structure']]
  }
  s[['file_structure']] <- file_structure

  # ------------- Whether to allow forked clusters ----------
  disable_fork_clusters <- s[['disable_fork_clusters']]
  if(!length(disable_fork_clusters)){ disable_fork_clusters <- FALSE }
  if(!is.logical(disable_fork_clusters)){ disable_fork_clusters <- as.logical(disable_fork_clusters) }
  if(isTRUE(disable_fork_clusters)){
    options(
      "dipsaus.no.fork" = TRUE,
      "dipsaus.cluster.backup" = "multisession"
    )
  } else {
    options("dipsaus.no.fork" = FALSE)
    disable_fork_clusters <- FALSE
  }
  s[['disable_fork_clusters']] <- disable_fork_clusters

  # ------------- 3D viewer templates ----------
  template_subject <- s[['threeBrain_template_subject']]
  if(length(template_subject) != 1 ||
     is.na(template_subject) ||
     !is.character(template_subject) ||
     !isTRUE(package_installed("threeBrain"))) {
    template_subject <- "N27"
  } else {
    # package_installed("threeBrain") checks whether
    # threeBrain is installed
    if(ns_check) {
      threeBrain <- asNamespace("threeBrain")
      temp_dir <- threeBrain$default_template_directory(check = FALSE)
      if(!dir.exists(file.path(temp_dir, template_subject))) {
        template_subject <- "N27"
      }
    }
  }
  options(threeBrain.template_subject = template_subject)
  s[['threeBrain_template_subject']] <- template_subject


  s
}

flush_conf <- function(s, conf_file){
  if( isTRUE(getOption("raveio.settings_readonly", FALSE)) ){
    return()
  }

  bak <- paste0(conf_file, strftime(Sys.time(), ".%y%m%d-%H%M%S.bak"))
  valid_backup <- FALSE
  if( file.exists(conf_file) ){
    # backup file
    file.copy(conf_file, bak)

    # check if backup file is valid
    valid_backup <- tryCatch({
      read_yaml(bak)
      TRUE
    }, error = function(e){
      FALSE
    })
  }

  info <- NULL
  if( valid_backup ){
    # bak exists and readable
    info <- trimws(readLines(bak), which = "right")
    info <- info[info != '']
  }

  f <- tempfile()
  save_yaml(s, f)

  cmp_info <- NULL
  cmp_info <- trimws(readLines(f), which = "right")
  cmp_info <- cmp_info[cmp_info != '']

  if( !is.null(cmp_info) && identical(cmp_info, info) ){
    unlink(f)
    unlink(bak)
    return()
  }

  try({
    file.copy(f, conf_file, overwrite = TRUE)
  }, silent = TRUE)

  # check if conf_file exists
  if( !file.exists(conf_file) ){
    # copy failed (might because of permission issues)
    warning("Unable to write configuration file to:\n  ", conf_file, "\nPermission denied?")
    unlink(f)
    unlink(bak)
    return()
  }

  # check if conf_file is valid yaml file
  valid <- tryCatch({
    read_yaml(conf_file)
    TRUE
  }, error = function(e){
    FALSE
  })

  if( valid ){
    unlink(f)
    unlink(bak)
    return()
  }

  # if invalid and backup file is valid
  if( valid_backup ){
    warning("Unable to update configurations. Rewind to previous version.")
    try({
      file.copy(bak, conf_file, overwrite = TRUE)
      unlink(bak)
    }, silent = TRUE)
    unlink(f)
    return()
  }

  # if invalid and backup file is also invalid
  if( file.exists(bak) ){
    warning("Unable to update configurations. The settings file is corrupted. \n",
            "Resetting to default settings. The original copy has been backed up at \n  ", bak)
    unlink(conf_file, force = TRUE)
    unlink(f)
    return()
  }

  warning("Unable to update configurations. The settings file is corrupted. ",
          "Resetting to default settings.")
  unlink(conf_file, force = TRUE)
  unlink(f)
  return()
}

load_setting <- function(reset_temp = TRUE){
  sess_str <- get('.session_string')
  tmp <- list()

  if(identical(Sys.getenv("RAVE_JOB_SESSION"), "1")) {
    s0 <- getOption("rave.use_settings", NULL)
  } else {
    s0 <- NULL
  }

  if(length(s0)) {
    s <- list_to_fastmap2(s0)
    s$session_string <- sess_str
    tmp <- s$..temp
    ns_check <- FALSE
  } else {
    ns_check <- TRUE
    s <- get0('.settings', ifnotfound = default_settings())
    s$session_string <- sess_str
    tmp <- s$..temp
    conf_path <- ravepipeline_config_dir()
    conf_file <- file.path(conf_path, 'settings.yaml')
    if(file.exists(conf_file)){
      tryCatch({
        load_yaml(conf_file, map = s)
      }, error = function(e){
        bak <- paste0(conf_file, strftime(Sys.time(), ".%y%m%d-%H%M%S.bak"))
        file.copy(conf_file, bak)
        unlink(conf_file, force = TRUE)
        warning("Configuration file is corrupted:\n  ", conf_file, "\nReset to default values. The original copy has been backed up at:\n  ", bak)
      })
    }
  }

  if( reset_temp ){
    s$..temp <- list()
  } else {
    s$..temp <- tmp
  }

  validate_settings(s, ns_check = ns_check)
  s
}


#' @name raveio-option
#' @title Set/Get 'RAVE' option
#' @description Persist settings on local configuration file
#' @param key character, option name
#' @param value character or logical of length 1, option value
#' @param default is key not found, return default value
#' @param all whether to reset all non-default keys
#' @param .save whether to save to local drive, internally used to temporary
#' change option. Not recommended to use it directly.
#' @param cfile file name in configuration path
#' @param temp when saving, whether the key-value pair should be considered
#' temporary, a temporary settings will be ignored when saving; when getting
#' options, setting \code{temp} to false will reveal the actual settings.
#' @returns \code{raveio_setopt} returns modified \code{value};
#' \code{raveio_resetopt} returns current settings as a list;
#' \code{raveio_confpath} returns absolute path for the settings file;
#' \code{raveio_getopt} returns the settings value to the given key, or
#' \code{default} if not found.
#' @seealso \code{R_user_dir}
#' @details \code{raveio_setopt} stores key-value pair in local path.
#' The values are persistent and shared across multiple sessions.
#' There are some read-only keys such as \code{"session_string"}. Trying to
#' set those keys will result in error.
#'
#' The following keys are reserved by 'RAVE':
#'
#' \describe{
#' \item{\code{data_dir}}{Directory path, where processed data are stored;
#' default is at home directory, folder \code{~/rave_data/data_dir}}
#' \item{\code{raw_data_dir}}{Directory path, where raw data files are stored,
#' mainly the original signal files and imaging files;
#' default is at home directory, folder \code{~/rave_data/raw_dir}}
#' \item{\code{max_worker}}{Maximum number of CPU cores to use; default
#' is one less than the total number of CPU cores}
#' \item{\code{mni_template_root}}{Directory path, where 'MNI' templates
#' are stored}
#' }
#'
#' \code{raveio_getopt} returns value corresponding to the keys. If key is
#' missing, the whole option will be returned.
#'
#' If set \code{all=TRUE}, \code{raveio_resetopt} resets all keys including
#' non-standard ones. However \code{"session_string"} will never reset.
#'
#' @section Side-Effects:
#' The following options will alter other packages and might cause changes in
#' behaviors:
#'
#' \describe{
#' \item{\code{'disable_fork_clusters'}}{This option will change the
#' \code{\link{options}} \code{'dipsaus.no.fork'} and
#' \code{'dipsaus.cluster.backup'}, which handles the parallel computing}
#' \item{\code{'threeBrain_template_subject'}}{This option will set and persist
#' option \code{'threeBrain.template_subject'}, which changes the default
#' group-level template brain.}
#' }
#'
#'
#' @examples
#'
#' # get one RAVE option
#' ncore <- raveio_getopt("max_worker")
#' print(ncore)
#'
#' # get all options
#' raveio_getopt()
#'
#' # set option
#' raveio_setopt("disable_fork_clusters", FALSE)
#'
NULL

#' @rdname raveio-option
#' @export
raveio_setopt <- function(key, value, .save = TRUE){

  stopifnot2(isTRUE(
    mode(value) %in% c('numeric', 'logical', 'character')
  ), msg = 'settings value must be numeric, character or logical')

  if(is.character(value) && length(value) > 1){
    stop('settings value must be length 1 for characters')
  }

  stopifnot2(!key %in% c('session_string'),
             msg = sprintf('Key %s is read-only', sQuote(key)))

  conf_path <- ravepipeline_config_dir()
  conf_file <- file.path(conf_path, 'settings.yaml')
  s <- load_setting(reset_temp = FALSE)

  previous <- s[[key]]
  s[[key]] <- value
  validate_settings(s)

  if( .save ){
    s$..temp[[key]] <- NULL
    s <- as.list(s)
    s <- s[!names(s) %in% c('session_string', '..temp')]
    dir_create2(conf_path)
    flush_conf(s, conf_file)
  } else {
    # temporarily set value and restore previous value because
    s$..temp[[key]] <- s[[key]]
    if(length(previous) && all(!is.na(previous))){
      s[[key]] <- previous
    }
  }

  invisible(value)
}

#' @rdname raveio-option
#' @export
raveio_resetopt <- function(all = FALSE){
  s <- get0('.settings', ifnotfound = default_settings())
  if(all){
    nms <- names(s)
    nms <- nms[!nms %in% c('session_string', '..temp')]
    .subset2(s, 'remove')(nms)
  }
  default_settings(s)
  validate_settings(s)

  # remove some temporary settings
  .subset2(s, 'remove')('tensor_temp_path')
  conf_path <- ravepipeline_config_dir()
  conf_file <- file.path(conf_path, 'settings.yaml')

  if(all && file.exists(conf_file)){
    unlink(conf_file)
  } else {
    dir_create2(conf_path)
    flush_conf(s, conf_file)
  }

  # validate again as temporary settings are removed
  validate_settings(s)

  invisible(as.list(s))
}


# get options whether the data directory is on network
# If enabled, then HDF5 files should be copied to local tempdir
# and read if there are multiiple reads from the same file
using_netdrive <- function(){
  if(raveio_getopt("using_netdrive", FALSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @rdname raveio-option
#' @export
raveio_getopt <- function(key, default = NA, temp = TRUE){
  s <- get0('.settings', ifnotfound = default_settings())
  tmp <- s$..temp

  if(missing(key)){
    s <- as.list(s)
    if(temp){
      for(nm in names(tmp)){
        s[[nm]] <- tmp[[nm]]
      }
    }
    return(s)
  }

  re <- NULL
  key_found <- FALSE
  if(temp && (key %in% names(tmp))){
    re <- tmp[[key]]
    key_found <- TRUE
  }
  if(!key_found && .subset2(s, 'has')(key)){
    re <- s[[key]]
    key_found <- TRUE
  }

  if(!key_found) {
    re <- default
  }

  try(silent = TRUE, expr = {
    if( identical(key, "max_worker") ) {
      if( re <= 0L ) {
        re <- 1L
      } else if(
        identical(key, "max_worker") &&
        (
          # identical(Sys.getenv("OMP_THREAD_LIMIT"), "2") ||
          identical(toupper(Sys.getenv("_R_CHECK_LIMIT_CORES_")), "TRUE")
        ) &&
        re > 2L
      ) {
        # Make sure using max 2 CPU cores on CRAN
        re <- 1L
      }
    }
  })

  re

}

#' @rdname raveio-option
#' @export
raveio_confpath <- function(cfile = 'settings.yaml'){
  normalizePath(ravepipeline_config_dir(cfile), mustWork = FALSE)
}




global_preferences <- function(name = "default", ..., .initial_prefs = list(),
                               .prefix_whitelist = NULL, .type_whitelist = NULL,
                               .overwrite = FALSE, .verbose = FALSE) {
  stopifnot2(
    grepl(pattern = "^[a-zA-Z0-9_-]+$",
          x = name),
    msg = "preference `name` must only contain letters (a-z), digits (0-9), underscore (_), and hyphen (-)"
  )
  name <- trimws(tolower(name))

  if(length(name) != 1 || is.na(name) || !nzchar(name) || grepl("(^\\.|[/\\\\])", name)) {
    stop("Invalid preference name [", name, "]. Must be non-hidden file name")
  }

  pref_path <- ravepipeline_config_dir("preferences", name)

  preference <- tryCatch({
    preference <- rds_map(pref_path)
    stopifnot(preference$is_valid)
    preference
  }, error = function(e) {
    if(file.exists(pref_path)) {
      unlink(pref_path, unlink(TRUE))
    }
    rds_map(pref_path)
  })

  prefix_whitelist <- unlist(.prefix_whitelist)
  type_whitelist <- unlist(.type_whitelist)

  validate_names <- function(nms, expected_length) {
    if(length(nms) != expected_length) {
      stop("Preference names cannot be blank")
    }
    if(expected_length == 0) { return() }
    if(any(nms == "")) {
      stop("Preference names cannot be blank")
    }
    # nms <- c("global.graphics.cex", "power_explorer.graphics.cex", "global.graphics.use_ggplot", "power_explorer.export.default_format")

    parsed <- t(sapply(nms, function(nm) {
      item <- strsplit(nm, ".", fixed = TRUE)[[1]]
      if(length(item) < 3) {
        stop("Invalid preference name `", nm, "`. \nPreference must have syntax [global/<module ID>].[type].[key]. \nFor example: `global.graphics.use_ggplot`, `power_explorer.export.default_format`, ...")
      }
      item <- c(item[c(1, 2)], paste(item[-c(1, 2)], collapse = "."))

      if(length(prefix_whitelist)) {
        if(!item[[1]] %in% prefix_whitelist) {
          stop(
            "Cannot set preference `",
            nm,
            "`: invalid prefix. Allowed prefix choices are: ",
            paste(sprintf("`%s`", prefix_whitelist), collapse = ", ")
          )
        }
      }

      if(length(type_whitelist)) {
        if(!item[[2]] %in% type_whitelist) {
          stop(
            "Cannot set preference `",
            nm,
            "`: invalid type. Allowed type choices are: ",
            paste(sprintf("`%s`", type_whitelist), collapse = ", ")
          )
        }
      }

      item
    }))

  }

  re <- list(
    get = preference$get,
    mget = preference$mget,

    set = function(key, value, signature) {
      validate_names(key, 1)
      if(is.null(value)) {
        # unset
        preference$remove(key)
      } else {
        if( missing(signature) ) {
          preference$set(key, value)
        } else {
          preference$set(key, value, signature)
        }
      }
      return(invisible(value))
    },
    mset = function(..., .list = NULL) {
      .list <- c(list(...), .list)
      nms <- names(.list)
      validate_names(nms, length(.list))
      is_null <- nms[sapply(nms, function(nm) { is.null(.list[[nm]]) })]
      if(length(is_null)) {
        preference$remove(is_null)
        nms <- nms[!nms %in% is_null]
      }
      if(length(nms)) {
        lapply(nms, function(nm){
          preference$set(nm, .list[[nm]])
        })
      }
      invisible(.list)
    },

    keys = function(include_signatures = FALSE) {
      if(include_signatures) {
        re <- preference$keys(TRUE)
        if(!length(re)) {
          re <- array(character(0L), c(0L, 2L))
        }
        rownames(re) <- NULL
        colnames(re) <- c("key", "signature")
      } else {
        re <- unname(preference$keys(FALSE))
      }
      re
    },
    has = preference$has,

    size = preference$size,
    remove = preference$remove,

    reset = preference$reset,
    destroy = preference$destroy
  )


  # avoid evaluating dots
  dot_names <- ...names()
  list_names <- names(.initial_prefs)
  nms <- c(dot_names, list_names)
  nms <- nms[!nms %in% ""]
  if(length(nms)) {
    if( !.overwrite ) {
      nms <- nms[ !nms %in% preference$keys() ]
    }
    if(length(nms)) {
      if( .verbose ) {
        catgl("Initializing the following preference(s): \n{ paste(nms, collapse = '\n') }", level = "DEBUG")
      }

      default_vals <- as.list(.initial_prefs[list_names %in% nms])
      nms <- nms[nms %in% dot_names]

      for(nm in nms) {
        default_vals[[nm]] <- ...elt(which(dot_names == nm))
      }
      re$mset(.list = default_vals)
    }
  }

  re
}

