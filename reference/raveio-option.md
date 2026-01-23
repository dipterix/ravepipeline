# Set/Get 'RAVE' option

Persist settings on local configuration file

## Usage

``` r
raveio_setopt(key, value, .save = TRUE)

raveio_resetopt(all = FALSE)

raveio_getopt(key, default = NA, temp = TRUE)

raveio_confpath(cfile = "settings.yaml")
```

## Arguments

- key:

  character, option name

- value:

  character or logical of length 1, option value

- .save:

  whether to save to local drive, internally used to temporary change
  option. Not recommended to use it directly.

- all:

  whether to reset all non-default keys

- default:

  is key not found, return default value

- temp:

  when saving, whether the key-value pair should be considered
  temporary, a temporary settings will be ignored when saving; when
  getting options, setting `temp` to false will reveal the actual
  settings.

- cfile:

  file name in configuration path

## Value

`raveio_setopt` returns modified `value`; `raveio_resetopt` returns
current settings as a list; `raveio_confpath` returns absolute path for
the settings file; `raveio_getopt` returns the settings value to the
given key, or `default` if not found.

## Details

`raveio_setopt` stores key-value pair in local path. The values are
persistent and shared across multiple sessions. There are some read-only
keys such as `"session_string"`. Trying to set those keys will result in
error.

The following keys are reserved by 'RAVE':

- `data_dir`:

  Directory path, where processed data are stored; default is at home
  directory, folder `~/rave_data/data_dir`

- `raw_data_dir`:

  Directory path, where raw data files are stored, mainly the original
  signal files and imaging files; default is at home directory, folder
  `~/rave_data/raw_dir`

- `max_worker`:

  Maximum number of CPU cores to use; default is one less than the total
  number of CPU cores

- `mni_template_root`:

  Directory path, where 'MNI' templates are stored

`raveio_getopt` returns value corresponding to the keys. If key is
missing, the whole option will be returned.

If set `all=TRUE`, `raveio_resetopt` resets all keys including
non-standard ones. However `"session_string"` will never reset.

## Side-Effects

The following options will alter other packages and might cause changes
in behaviors:

- `'disable_fork_clusters'`:

  This option will change the
  [`options`](https://rdrr.io/r/base/options.html) `'dipsaus.no.fork'`
  and `'dipsaus.cluster.backup'`, which handles the parallel computing

- `'threeBrain_template_subject'`:

  This option will set and persist option
  `'threeBrain.template_subject'`, which changes the default group-level
  template brain.

## See also

`R_user_dir`

## Examples

``` r
# get one RAVE option
ncore <- raveio_getopt("max_worker")
print(ncore)
#> [1] 3

# get all options
raveio_getopt()
#> $threeBrain_template_subject
#> [1] "N27"
#> 
#> $session_string
#> [1] "1871657b86781f1"
#> 
#> $py_virtualenv
#> [1] ""
#> 
#> $py3_path
#> [1] ""
#> 
#> $py2_path
#> [1] ""
#> 
#> $matlab_path
#> [1] "/Applications/MATLAB_R2016b.app/bin"
#> 
#> $suma_nodes_per_electrodes
#> [1] 42
#> 
#> $server_time_zone
#> [1] "America/Chicago"
#> 
#> $max_mem
#> 15.6159820556641
#> $disable_fork_clusters
#> [1] FALSE
#> 
#> $max_worker
#> [1] 3
#> 
#> $disable_startup_speed_check
#> [1] FALSE
#> 
#> $drive_speed
#> [1] 50 20
#> 
#> $image_height
#> [1] 768
#> 
#> $image_width
#> [1] 1280
#> 
#> $fast_cache
#> [1] TRUE
#> 
#> $test_mode
#> [1] FALSE
#> 
#> $delay_input
#> [1] 20
#> 
#> $module_lookup_file
#> [1] "~/rave_modules/modules.csv"
#> 
#> $module_root_dir
#> [1] "~/rave_modules/"
#> 
#> $file_structure
#> [1] "native"
#> 
#> $bids_data_dir
#> [1] "/home/runner/rave_data/bids_dir/"
#> 
#> $data_dir
#> [1] "/home/runner/rave_data/data_dir/"
#> 
#> $raw_data_dir
#> [1] "/home/runner/rave_data/raw_dir/"
#> 
#> $verbose_level
#> [1] "DEBUG"
#> 
#> $tensor_temp_path
#> [1] "~/rave_data/cache_dir/"
#> 
#> $..temp
#> list()
#> 

# set option
raveio_setopt("disable_fork_clusters", FALSE)
```
