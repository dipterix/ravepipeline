# Round-trip tests for `rave-brain` / `multi-rave-brain` serialization.
#
# The subject used here is resolved at run time so it can be swapped without
# touching any test body: set `RAVEPIPELINE_TEST_BRAIN_SUBJECT` to override.

testthat::skip_on_cran()
subject_id <- Sys.getenv("RAVEPIPELINE_TEST_BRAIN_SUBJECT", unset = "YAEL/CIT168")

ravepipeline <- asNamespace("ravepipeline")

test_subject <- function() {
  tryCatch(
    ravepipeline$call_ravecore_fun("as_rave_subject", subject_id, strict = FALSE),
    error = function(e) { NULL }
  )
}

# Every restore path here reaches `ravecore` (or `raveio`), which is downstream
# of this package and so cannot be a `Suggests`; without it there is nothing to
# exercise. Guarded for the whole file, not per test, because `R CMD check`
# runs in a library where neither is visible.
testthat::skip_if_not_installed("threeBrain")
testthat::skip_if(
  !ravepipeline$package_installed("ravecore") && !ravepipeline$package_installed("raveio"),
  "neither `ravecore` nor `raveio` is installed"
)

# Skips unless the subject's FreeSurfer folder is available too. Returns the
# subject.
skip_unless_brain_available <- function() {
  subject <- test_subject()
  skip_if(is.null(subject), sprintf("subject [%s] is unavailable", subject_id))

  fs_path <- subject$freesurfer_path
  skip_if(
    length(fs_path) != 1 || is.na(fs_path) || !dir.exists(fs_path),
    sprintf("subject [%s] has no FreeSurfer folder", subject_id)
  )
  subject
}

# Whatever streamline groups the subject happens to ship. The default
# `default/*` group is often absent, and streamlines are part of what must
# round-trip, so load them explicitly.
add_available_streamlines <- function(brain) {
  root <- file.path(brain$base_path, "streamline")
  if (length(root) != 1 || !dir.exists(root)) { return(brain) }

  groups <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  groups <- groups[nzchar(groups)]
  if (!length(groups)) { return(brain) }

  suppressWarnings(tryCatch(
    brain$add_streamline(sprintf("%s/*", groups)),
    error = function(e) { NULL }
  ))
  brain
}

# Building a brain is expensive; build it once and reuse across tests.
test_brain <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      brain <- ravepipeline$call_ravecore_fun(
        "rave_brain", subject_id, include_electrodes = FALSE)
      cached <<- add_available_streamlines(brain)
    }
    cached
  }
})

# A small synthetic montage so electrode assertions do not depend on whether
# the subject ships an electrode table.
test_electrode_table <- function(n = 4L) {
  data.frame(
    Electrode = seq_len(n),
    Coord_x = seq_len(n) * 1.5,
    Coord_y = seq_len(n) * -2.5,
    Coord_z = seq_len(n) * 0.5,
    Label = sprintf("Ch%d", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

annotation_key <- function(brain) {
  annots <- brain$annotation_types
  if (!is.data.frame(annots) || !nrow(annots)) { return(character(0L)) }
  sort(sprintf("%s@%s", annots$name, annots$surface))
}

# `names()` of an empty list is NULL, which `expect_setequal` rejects
geom_names <- function(x) {
  as.character(names(x))
}

expect_brain_round_trip <- function(original, restored) {
  expect_true(inherits(restored, "rave-brain"))
  expect_identical(restored$subject_code, original$subject_code)
  expect_setequal(geom_names(restored$surfaces), geom_names(original$surfaces))
  expect_setequal(geom_names(restored$atlases), geom_names(original$atlases))
  expect_setequal(geom_names(restored$streamlines), geom_names(original$streamlines))
  expect_equal(annotation_key(restored), annotation_key(original))
  expect_equal(restored$electrodes$raw_table, original$electrodes$raw_table)
  expect_equal(restored$electrodes$value_table, original$electrodes$value_table)
}

refhook_round_trip <- function(object) {
  raw <- serialize(object, NULL, refhook = ravepipeline$rave_serialize_refhook)
  unserialize(raw, refhook = ravepipeline$rave_unserialize_refhook)
}

format_round_trip <- function(object) {
  serializer <- ravepipeline$target_format("rave-brain")
  path <- tempfile(fileext = ".rds")
  on.exit({ unlink(path) }, add = TRUE)
  serializer$write(object = object, path = path)
  serializer$read(path)
}

test_that("single brain round-trips through the refhook", {
  skip_unless_brain_available()
  brain <- test_brain()
  brain$set_electrodes(test_electrode_table())

  expect_brain_round_trip(brain, refhook_round_trip(brain))
})

test_that("single brain round-trips through the `rave-brain` target format", {
  skip_unless_brain_available()
  brain <- test_brain()
  brain$set_electrodes(test_electrode_table())

  expect_brain_round_trip(brain, format_round_trip(brain))
})

test_that("electrode values survive the round trip", {
  skip_unless_brain_available()
  brain <- test_brain()
  brain$set_electrodes(test_electrode_table())
  brain$set_electrode_values(data.frame(
    Electrode = 1:4,
    Subject = brain$subject_code,
    MyValue = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  ))

  restored <- refhook_round_trip(brain)
  expect_brain_round_trip(brain, restored)
  expect_true(is.data.frame(restored$electrodes$value_table))
  expect_setequal(restored$electrodes$value_table$MyValue, c(0.1, 0.2, 0.3, 0.4))
})

test_that("multi brain round-trips through both paths", {
  skip_unless_brain_available()
  threeBrain <- asNamespace("threeBrain")
  brain <- test_brain()
  brain$set_electrodes(test_electrode_table())
  merged <- threeBrain$merge_brain(brain)

  for (restored in list(refhook_round_trip(merged), format_round_trip(merged))) {
    expect_true(inherits(restored, "multi-rave-brain"))
    expect_setequal(restored$subject_codes, merged$subject_codes)
    expect_identical(restored$template_subject, merged$template_subject)
    expect_setequal(
      names(restored$template_object$surfaces),
      names(merged$template_object$surfaces)
    )
    expect_brain_round_trip(
      merged$objects[[brain$subject_code]],
      restored$objects[[brain$subject_code]]
    )
  }
})

test_that("a brain built outside `ravecore` restores through `base_path`", {
  subject <- skip_unless_brain_available()
  threeBrain <- asNamespace("threeBrain")

  brain <- threeBrain$threeBrain(
    path = subject$freesurfer_path,
    subject_code = subject$subject_code,
    surface_types = "pial"
  )
  skip_if(is.null(brain), "cannot build a bare `threeBrain` object")

  params <- ravepipeline$brain_marshal(brain)
  expect_true(is.na(params$project_name))
  expect_true(length(params$base_path) == 1 && nzchar(params$base_path))

  expect_brain_round_trip(brain, refhook_round_trip(brain))
})

test_that("a `ravecore` brain records no `base_path`", {
  skip_unless_brain_available()
  params <- ravepipeline$brain_marshal(test_brain())

  # `ravecore` resolves the subject directory itself; a recorded path would go
  # stale the moment the user moves their data directory
  expect_null(params$base_path)
  expect_identical(params$project_name, "YAEL")
})

test_that("writing a non-brain object degrades instead of erroring", {
  serializer <- ravepipeline$target_format("rave-brain")
  path <- tempfile(fileext = ".rds")
  on.exit({ unlink(path) }, add = TRUE)

  expect_warning(serializer$write(object = 1:10, path = path))
  expect_null(serializer$read(path))
})

test_that("legacy parameter lists without a version stamp still read", {
  # Shape written by `rave_serialize_impl.rave-brain` before this change
  legacy_a <- structure(
    list(
      project_name = "NoSuchProject",
      subject_code = "NoSuchSubject",
      use_141 = FALSE,
      usetemplateifmissing = FALSE,
      surface_types = list(list(
        name = "pial", annotations = NULL, vertex_color_types = "sulc")),
      atlas_types = list(),
      electrode_table = test_electrode_table(),
      has_prototypes = FALSE,
      native_subject = TRUE
    ),
    class = c("rave_serialized_rave-brain", "rave_serialized")
  )

  # Shape written by the `rave-brain` target format before this change
  legacy_b <- list(
    class = "rave-brain",
    params = list(
      project_name = "NoSuchProject",
      subject_code = "NoSuchSubject",
      use_141 = FALSE,
      usetemplateifmissing = FALSE,
      surfaces = c("pial", "white"),
      electrode_table = test_electrode_table(),
      electrode_values = NULL
    )
  )

  # The subject does not exist, so both must take the legacy branch and
  # degrade to `NULL` with a warning rather than error out
  expect_null(suppressWarnings(ravepipeline$rave_unserialize_impl(legacy_a)))

  path <- tempfile(fileext = ".rds")
  on.exit({ unlink(path) }, add = TRUE)
  saveRDS(legacy_b, path, version = 3L)
  expect_null(suppressWarnings(ravepipeline$target_format("rave-brain")$read(path)))
})
