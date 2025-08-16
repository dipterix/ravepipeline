
pipeline_report_list <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
    # out_dir = file.path(pipe_dir, "reports")
) {

  pipe_dir <- activate_pipeline(pipe_dir)

  # DIPSAUS DEBUG START
  # pipe_dir <- activate_pipeline(pipe_dir, debug = TRUE)

  # save_yaml(
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
        fork_policy = "default"
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
    if(length(item$fork_policy) == 0) {
      item$fork_policy <- "default"
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
    name, output_format = "auto", clean = FALSE,
    theme = "flatly", ..., code_folding = TRUE,
    self_contained = TRUE, toc = TRUE, toc_depth = 3L,
    toc_float = TRUE,
    output_dir = NULL, work_dir = output_dir, attributes = list(),
    pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {

  report <- pipeline_report_by_name(name = name, pipe_dir = pipe_dir)
  pipe_dir <- report$pipeline_path
  pipeline_name <- attr(pipe_dir, "target_name")

  pipeline <- pipeline_from_path(pipe_dir)

  # output_name <- format(x = Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  datetime <- format(x = Sys.time(), format = "%y%m%dT%H%M%S")
  report_filename <- sprintf(
    "report-%s_datetime-%s_%s",
    gsub("[_\\.\\-]", "", report$name),
    datetime,
    pipeline_name
  )

  attributes <- as.list(attributes)
  attributes$type <- "widget"
  attributes$module <- "standalone_report"
  attributes$report_name <- attributes$name
  attributes$report_module <- pipeline_name
  attributes$report_filename <- report_filename
  attributes <- as.list(attributes[!names(attributes) %in% ""])

  if(length(output_dir) != 1 || is.na(output_dir)) {
    output_dir <- file.path(pipe_dir, "reports")
  } else {
    output_dir <- file.path(output_dir, report_filename)
  }

  if(length(work_dir) != 1 || is.na(work_dir)) {
    # work_dir <- file.path(cache_root(), "ravepipeline-reports", report_filename)
    work_dir <- file.path(tempdir(), report_filename)
  }
  dir_create2(work_dir)

  fork_policy <- report$fork_policy
  if(length(fork_policy) == 1) {
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


  output_options <- list(
    theme = theme,
    code_folding = code_folding,
    self_contained = self_contained,
    toc = toc,
    toc_depth = as.integer(toc_depth),
    toc_float = toc_float
  )
  custom_css <- file.path(pipeline$pipeline_path, "report_styles.css")
  if(file.exists(custom_css)) {
    output_options$css <- "report_styles.css"
  }

  if(identical(output_format, "auto")) {
    if(package_installed("distill")) {
      output_format <- "distill::distill_article"
    } else {
      output_format <- "html_document"
    }
  }

  workdir <- dirname(report_template_path)
  call_args <- list(
    input = report_template_path,
    output_format = output_format,
    output_file = file.path(work_dir, "report.html"),
    output_options = output_options,
    intermediates_dir = file.path(work_dir, "intermediate"),
    knit_root_dir = workdir,
    clean = clean,
    ...
  )

  job_id <- start_job(
    fun = function(call_args, source_path, output_dir, attributes) {

      Sys.setenv("RAVE_REPORT_ACTIVE" = "true")
      on.exit({ Sys.unsetenv("RAVE_REPORT_ACTIVE") }, add = TRUE, after = FALSE)

      rmarkdown <- asNamespace("rmarkdown")
      do.call(rmarkdown$render, call_args)

      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

      # copy work_dir to output_dir
      source_path <- normalizePath(source_path, winslash = "/")
      output_dir <- normalizePath(output_dir, winslash = "/")

      # To avoid same-path copy
      if(output_dir != source_path) {
        try({
          file.copy(
            from = source_path,
            to = output_dir,
            overwrite = TRUE,
            recursive = TRUE
          )

          unlink(source_path, recursive = TRUE, force = TRUE)
        })
      }

      Sys.unsetenv("RAVE_REPORT_ACTIVE")

      path <- file.path(output_dir, basename(source_path), "report.html")

      structure(
        path,
        params = attributes,
        class = c("fs_path", "character")
      )
    },
    fun_args = list(
      call_args = call_args,
      source_path = work_dir,
      output_dir = output_dir,
      attributes = attributes
    ),
    packages = "rmarkdown",
    workdir = workdir,
    name = report_filename
  )

  return(job_id)
}
