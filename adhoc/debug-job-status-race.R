# Reproduce "Job [<id>] has not been prepared yet." from `resolve_job()` and
# `lapply_jobs()`: bug "Parallel jobs fail with 'has not been prepared yet'" in
# ../rave-pipelines/BUGS.md, fixed in `R/jobs.R` for 0.2.0.9.
# Run from the package root: Rscript adhoc/debug-job-status-race.R

# The installed package may already have the fix, so the job functions are
# loaded from `R/jobs.R` at `ref`, on top of the installed package. "52c3730"
# is the last commit before the fix; NULL tests the working tree (the fix)
ref <- "52c3730"

stopifnot(file.exists("R/jobs.R"))
code <- if (is.null(ref)) {
  readLines("R/jobs.R")
} else {
  system2("git", c("show", paste0(ref, ":R/jobs.R")), stdout = TRUE)
}
jobs <- new.env(parent = asNamespace("ravepipeline"))
eval(parse(text = code), envir = jobs)
cat("R/jobs.R from:", if (is.null(ref)) "working tree" else ref, "\n")

# Part 1: the cause. A job reports its progress by rewriting `status.rds` with
# `save_job_status()`. Before the fix, it copies a temporary file over it, and
# `file.copy()` empties the target before appending to it, so a reader can find
# the file empty or half written. Another process rewrites a status file 3000
# times while this one keeps reading it the way `get_job_status()` does.
# Before the fix: many failed reads (3373 of 30,278 in BUGS.md). After: none
status_path <- file.path(tempfile("job-status-"), "status.rds")
dir.create(dirname(status_path))
saveRDS(list(status = 2, current_time = Sys.time()), status_path)
writer <- callr::r_bg(
  function(save_job_status, path) {
    status <- list(status = 2, current_time = Sys.time())
    for (i in seq_len(3000)) {
      save_job_status(status, path)
    }
  },
  args = list(save_job_status = jobs$save_job_status, path = status_path)
)
reads <- 0
failed <- 0
while (writer$is_alive()) {
  read <- tryCatch(suppressWarnings(readRDS(status_path)),
                   error = function(e) NULL)
  reads <- reads + 1
  failed <- failed + is.null(read)
}
invisible(writer$get_result()) # stops here if the writer failed
cat(sprintf("Part 1: %d of %d reads failed\n", failed, reads))
unlink(dirname(status_path), recursive = TRUE)

# Part 2: the effect. Before the fix, `get_job_status()` reports a job as
# missing (status -2) when 5 reads 10 ms apart fail, and `resolve_job()` stops
# at once, so a worker paused inside `file.copy()` for about 50 ms fails
# `lapply_jobs()`. Emptying a running job's status file leaves it the way such
# a worker does. (The job runs the installed package; only this process, the
# reader, runs `ref`.)
# Before the fix: "Job [<id>] has not been prepared yet." After: "job finished"
job <- jobs$start_job(function() {
  Sys.sleep(1)
  "job finished"
})
while (jobs$check_job(job)$status %in% c(0, 1)) {
  Sys.sleep(0.01)
}
stopifnot(jobs$check_job(job)$status == 2) # the job is running
invisible(file.create(
  file.path(jobs$get_job_path(job, check = FALSE), "status.rds")
))
result <- tryCatch(jobs$resolve_job(job), error = function(e) {
  paste("Error:", conditionMessage(e))
})
cat("Part 2: resolve_job() gives:", result, "\n")
