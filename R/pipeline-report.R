
pipeline_report_list <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
    # out_dir = file.path(pipe_dir, "reports")
) {

  pipe_dir <- activate_pipeline(pipe_dir)

  # DIPSAUS DEBUG START
  # pipe_dir <- activate_pipeline(pipe_dir, debug = TRUE)

  # raveio::save_yaml(
  #   file = '/Users/dipterix/Dropbox (Personal)/projects/rave-pipelines/modules/power_explorer/report-list.yaml',
  #   list(
  #     list(
  #       name = "report-id",
  #       label = "My Report Title",
  #       entry = "main.Rmd",
  #       fork_policy = NULL
  #     )
  #   )
  # )

  # check the possible reports
  report_config <- file.path(pipe_dir, "report-list.yaml")
  if(!file.exists(report_config)) {
    return(list())
  }
  report_config <- as.list(yaml::read_yaml(report_config))

  report_config <- lapply(report_config, function(item) {

    if(is.character(item)) {
      if(length(item) != 1 || is.na(item) || !nzchar(item)) { return(NULL) }
      entry_path <- file.path(pipe_dir, item)
      if(!file.exists(entry_path)) { return(NULL) }
      return(list(
        name = basename(entry_path),
        label = basename(entry_path),
        entry = item,
        fork_policy = NULL
      ))
    }
    if(!is.list(item) || length(item$entry) != 1 || is.na(item$entry) || !is.character(item$entry)) { return(NULL) }
    entry_path <- file.path(pipe_dir, item$entry)
    if(!file.exists(entry_path)) { return(NULL) }

    if(is.null(item$name)) {
      item$name <- basename(entry_path)
    }
    if(is.null(item$label)) {
      item$label <- basename(entry_path)
    }
    return(item)
  })

  report_config <- report_config[!vapply(report_config, is.null, FALSE)]
  list(
    pipeline_path = pipe_dir,
    report_configurations = report_config
  )

}

pipeline_report_by_name <- function(
    name,
    pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {

  report_list <- pipeline_report_list(pipe_dir = pipe_dir)
  pipe_dir <- report_list$pipeline_path
  reports <- report_list$report_configurations
  if(!length(reports)) {
    stop("This pipeline does not have any report template available.")
  }
  report_names <- unlist(lapply(reports, "[[", "name"))
  report_idx <- which(report_names %in% name)

  if(!length(report_idx)) {
    stop("Unable to find report ", sQuote(name), ". Available reports are: ", paste(sQuote(report_names), collapse = ", "), ".")
  }

  report <- reports[[report_idx[[1]]]]
  report$pipeline_path <- pipe_dir
  return(report)
}

pipeline_report_generate <- function(
    name, output_format = "html_document", clean = FALSE, ...,
    output_dir = NULL, work_dir = NULL,
    pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {

  report <- pipeline_report_by_name(name = name, pipe_dir = pipe_dir)
  pipe_dir <- report$pipeline_path
  pipeline_name <- attr(pipe_dir, "target_name")

  # output_name <- format(x = Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  datetime <- format(x = Sys.time(), format = "%Y%m%dT%H%M%S")
  report_filename <- sprintf(
    "report-%s_datetime-%s_%s",
    gsub("[_\\.\\-]", "", report$name),
    datetime,
    pipeline_name
  )

  if(length(output_dir) != 1 || is.na(output_dir)) {
    output_dir <- file.path(pipe_dir, "reports")
  }

  if(length(work_dir) != 1 || is.na(work_dir)) {
    # work_dir <- file.path(cache_root(), "ravepipeline-reports", report_filename)
    work_dir <- file.path(tempdir(), report_filename)
  }
  dir_create2(work_dir)

  fork_policy <- report$fork_policy
  if(length(fork_policy) == 1) {
    pipeline <- pipeline_from_path(pipe_dir)
    pipeline <- pipeline$fork(path = file.path(work_dir, "pipeline"))
  }

  report_template_path <- file.path(pipeline$pipeline_path, report$entry)
  if(!file.exists(report_template_path)) {
    report_template_path_orig <- file.path(report$pipeline_path, report$entry)
    file.copy(report_template_path_orig, report_template_path, overwrite = TRUE)
  }

  # output_format = rmarkdown::html_document(
  #   toc = TRUE, toc_depth = 3, toc_float = list(collapsed=TRUE),
  #   df_print = 'kable',
  #   theme = 'spacelab'
  # )

  workdir <- dirname(report_template_path)
  call_args <- list(
    input = report_template_path,
    output_format = output_format,
    output_file = file.path(work_dir, "report.html"),
    intermediates_dir = file.path(work_dir, "intermediate"),
    knit_root_dir = workdir,
    clean = clean,
    ...
  )

  job_id <- start_job(
    fun = function(call_args, source_path, output_dir) {
      rmarkdown <- asNamespace("rmarkdown")
      do.call(rmarkdown$render, call_args)

      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

      # copy work_dir to output_dir
      file.copy(
        from = source_path,
        to = output_dir,
        overwrite = TRUE,
        recursive = TRUE
      )

      unlink(source_path, recursive = TRUE, force = TRUE)

      file.path(output_dir, basename(source_path), "report.html")
    },
    fun_args = list(
      call_args = call_args,
      source_path = work_dir,
      output_dir = output_dir
    ),
    packages = "rmarkdown",
    workdir = workdir,
    name = report_filename
  )

  return(job_id)
}
