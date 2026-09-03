# Canonical marshal/unmarshal for `rave-brain` and `multi-rave-brain` objects.
#
# A brain is an `R6` environment holding cached file handles, so it cannot be
# saved verbatim: it is marshaled into a small parameter list and rebuilt on
# read. Both serialization paths delegate here so the two cannot drift apart:
#
#   * `rave_serialize_impl` / `rave_unserialize_impl` (R/serialize.R), used by
#     the default target format, `jobs.R`, and `FileMap`
#   * the `rave-brain` target format (R/pipeline-serializers-brain.R)
#
# Every marshaled list records `threeBrain_version`. Its absence marks a list
# written before this schema existed and routes to the `*_legacy` readers.


# ---- marshal ---------------------------------------------------------------

brain_marshal <- function(brain) {
  if (!inherits(brain, "rave-brain")) { return(NULL) }

  constructor_params <- brain$meta$constructor_params
  project_name <- constructor_params$project_name
  subject_code <- constructor_params$subject_code

  if (length(project_name) == 1 && !is.na(project_name) &&
      length(subject_code) == 1 && !is.na(subject_code)) {
    # `ravecore` resolves the subject directory itself, so `base_path` is not
    # recorded: it would go stale as soon as the data directory moves
    params <- list(
      project_name = as.character(project_name),
      subject_code = as.character(subject_code),
      usetemplateifmissing = isTRUE(as.logical(constructor_params$usetemplateifmissing))
    )
  } else {
    # built outside `ravecore` (e.g. `threeBrain::threeBrain`) or a template
    # object; `base_path` is the only meaningful locator
    params <- list(
      project_name = NA_character_,
      subject_code = brain$subject_code,
      usetemplateifmissing = FALSE,
      base_path = brain$base_path
    )
  }

  params$surface_types <- names(brain$surfaces)
  params$annotation_types <- brain_annotation_table(brain)
  params$atlas_types <- brain_atlas_list(brain)
  params$streamline_types <- names(brain$streamlines)

  params$electrode_table <- brain$electrodes$raw_table
  params$electrode_values <- brain$electrodes$value_table
  params$has_prototypes <- length(brain$electrodes$objects2) > 0

  params$threeBrain_version <- threeBrain_version()
  params
}

multibrain_marshal <- function(object) {
  if (!inherits(object, "multi-rave-brain")) { return(NULL) }

  individual_params <- lapply(object$objects, brain_marshal)
  individual_params <- as.list(
    individual_params[!vapply(individual_params, is.null, FUN.VALUE = logical(1))])

  list(
    template_subject = object$template_subject,
    template_params = brain_marshal(object$template_object),
    individual_params = individual_params,
    threeBrain_version = threeBrain_version()
  )
}

threeBrain_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("threeBrain")),
    error = function(e) { NA_character_ }
  )
}

# `data.frame(name, surface)` of every annotation attached to a surface
brain_annotation_table <- function(brain) {
  empty <- data.frame(name = character(0L), surface = character(0L),
                      stringsAsFactors = FALSE)

  annots <- tryCatch(brain$annotation_types, error = function(e) { NULL })
  if (is.data.frame(annots) && all(c("name", "surface") %in% names(annots))) {
    if (!nrow(annots)) { return(empty) }
    return(data.frame(
      name = as.character(annots$name),
      surface = as.character(annots$surface),
      stringsAsFactors = FALSE
    ))
  }

  # older `threeBrain` has no `annotation_types` binding: read the surfaces
  tables <- lapply(names(brain$surfaces), function(stype) {
    alist <- brain$surfaces[[stype]]$group$group_data$annotation_list
    if (!length(alist)) { return(NULL) }
    data.frame(name = as.character(alist), surface = stype, stringsAsFactors = FALSE)
  })
  tables <- tables[!vapply(tables, is.null, FUN.VALUE = logical(1))]
  if (!length(tables)) { return(empty) }
  do.call("rbind", unname(tables))
}

# List of `list(name, color_format, trans_space_from, path)`. `path` is the
# cached volume file, used to re-register an atlas whose name no longer
# resolves against the FreeSurfer folder.
brain_atlas_list <- function(brain) {
  atlases <- lapply(names(brain$atlases), function(aname) {
    atlas <- brain$atlases[[aname]]
    if (is.null(atlas)) { return(NULL) }
    list(
      name = aname,
      color_format = brain_atlas_color_format(atlas),
      trans_space_from = brain_atlas_trans_space(atlas),
      path = atlas$group$group_data$volume_data$absolute_path
    )
  })
  atlases[!vapply(atlases, is.null, FUN.VALUE = logical(1))]
}

brain_atlas_color_format <- function(atlas) {
  color_format <- tryCatch(atlas$object$color_format, error = function(e) { NULL })
  if (length(color_format) != 1 || is.na(color_format)) { return("RGBAFormat") }
  as.character(color_format)
}

brain_atlas_trans_space <- function(atlas) {
  trans_space <- tryCatch(atlas$object$trans_space_from, error = function(e) { NULL })
  if (length(trans_space) != 1 || is.na(trans_space)) { return("model") }
  as.character(trans_space)
}


# ---- unmarshal -------------------------------------------------------------

# `brain` restores onto a live brain instead of constructing a new one; that is
# how the template object of a merged brain is filled back in.
brain_unmarshal <- function(params, brain = NULL) {
  if (inherits(params, "rave-brain")) { return(params) }
  if (!length(params)) { return(brain) }
  params <- unclass(params)

  if (!length(params$threeBrain_version)) {
    return(brain_unmarshal_legacy(params, brain = brain))
  }

  tryCatch({
    if (!inherits(brain, "rave-brain")) {
      brain <- brain_construct(params)
    }
    if (!inherits(brain, "rave-brain")) {
      warning(sprintf("Cannot import 3D model - [%s]",
                      paste(params$subject_code, collapse = "")))
      return(NULL)
    }
    brain_restore_geometries(brain, params)
    brain_restore_electrodes(brain, params)
    brain
  }, error = function(e) {
    warning(brain_import_failure(params, e))
    NULL
  })
}

# A plain message, never the condition object: `warning(<error condition>)`
# re-signals it as an error, which escapes `suppressWarnings` and turns a
# soft failure into a hard one for any caller handling errors.
brain_import_failure <- function(params, e) {
  sprintf("Cannot import 3D model - [%s]. Reason: %s",
          paste(params$subject_code, collapse = ""),
          paste(conditionMessage(e), collapse = ""))
}

multibrain_unmarshal <- function(params) {
  if (inherits(params, "multi-rave-brain")) { return(params) }
  if (!length(params)) { return(NULL) }
  params <- unclass(params)

  if (!length(params$threeBrain_version)) {
    return(multibrain_unmarshal_legacy(params))
  }

  template_params <- params$template_params

  blist <- lapply(params$individual_params, brain_unmarshal)
  blist <- as.list(blist[!vapply(blist, is.null, FUN.VALUE = logical(1))])

  template_subject <- params$template_subject
  if (!length(template_subject)) {
    template_subject <- template_params$subject_code
  }

  merged <- multibrain_merge(blist, template_subject, template_params)
  if (is.null(merged)) { return(NULL) }

  # Restoring the template happens outside the merge above: a merge that had to
  # fall back must still get its atlases, annotations, and electrodes back.
  if (length(template_params)) {
    template_params <- multibrain_remap_template(template_params, merged$template_object)
    template_object <- brain_unmarshal(template_params, brain = merged$template_object)
    if (inherits(template_object, "rave-brain")) {
      merged$template_object <- template_object
    }
  }
  merged
}

# `merge_brain` gained template arguments over time; try the richest call
# first and degrade instead of losing the merge altogether
multibrain_merge <- function(blist, template_subject, template_params) {
  threeBrain <- require_package("threeBrain", return_namespace = TRUE)

  args_full <- drop_nulls(list(
    .list = blist,
    template_subject = template_subject,
    template_surface_types = brain_type_names(template_params$surface_types),
    template_atlas_types = brain_type_names(template_params$atlas_types),
    template_annotation_types = brain_type_names(template_params$annotation_types),
    template_streamline_types = brain_type_names(template_params$streamline_types)
  ))
  args_basic <- args_full[names(args_full) %in%
                            c(".list", "template_subject", "template_surface_types")]

  for (args in list(args_full, args_basic, list(.list = blist))) {
    merged <- tryCatch(
      do.call(threeBrain$merge_brain, args),
      error = function(e) { NULL }
    )
    if (inherits(merged, "multi-rave-brain")) { return(merged) }
  }

  warning("Cannot import merged 3D models. Returning `NULL`.")
  NULL
}

# The template subject may differ from the one recorded at write time (the
# `threeBrain.template_subject` option can change); point the electrode tables
# at whichever template was actually built.
multibrain_remap_template <- function(template_params, template_object) {
  old_code <- template_params$subject_code
  new_code <- template_object$subject_code
  if (length(old_code) != 1 || length(new_code) != 1 || identical(old_code, new_code)) {
    return(template_params)
  }

  etable <- template_params$electrode_table
  if (is.data.frame(etable) && nrow(etable) && length(etable$Subject)) {
    etable$Subject[etable$Subject == old_code] <- new_code
    template_params$electrode_table <- etable
  }

  vtable <- template_params$electrode_values
  if (is.data.frame(vtable) && nrow(vtable) && length(vtable$Subject)) {
    vtable$Subject[vtable$Subject == old_code] <- new_code
    template_params$electrode_values <- vtable
  }

  template_params$subject_code <- new_code
  template_params
}

brain_construct <- function(params) {
  subject_code <- params$subject_code
  surface_types <- brain_type_names(params$surface_types)
  project_name <- params$project_name

  if (length(project_name) == 1 && !is.na(project_name) && nzchar(project_name)) {
    subject_id <- sprintf("%s/%s", project_name, subject_code)
    usetemplateifmissing <- isTRUE(params$usetemplateifmissing)

    # geometries are replayed below, so nothing but the surfaces is loaded here
    brain <- tryCatch({
      call_ravecore_fun(
        "rave_brain",
        subject_id,
        surfaces = surface_types,
        overlays = NULL,
        annotations = NULL,
        streamlines = NULL,
        usetemplateifmissing = usetemplateifmissing,
        include_electrodes = FALSE
      )
    }, error = function(e) {
      call_ravecore_fun(
        "rave_brain",
        subject_id,
        overlays = NULL,
        usetemplateifmissing = usetemplateifmissing,
        include_electrodes = FALSE
      )
    })
    if (inherits(brain, "rave-brain")) { return(brain) }
  }

  base_path <- params$base_path
  if (length(base_path) == 1 && !is.na(base_path) && dir.exists(base_path)) {
    threeBrain <- require_package("threeBrain", return_namespace = TRUE)
    brain <- threeBrain$threeBrain(
      path = base_path,
      subject_code = subject_code,
      surface_types = surface_types,
      atlas_types = NULL,
      annotation_types = NULL,
      streamline_types = NULL
    )
    if (inherits(brain, "rave-brain")) { return(brain) }
  }

  NULL
}

# Replays surfaces, atlases, annotations, and streamlines. Each block is
# isolated so one missing file does not cost the others. Replaying (rather than
# passing these to the constructor) is what lets an already-built template
# object take the same path.
brain_restore_geometries <- function(brain, params) {

  tryCatch({
    missing_surfaces <- setdiff(brain_type_names(params$surface_types),
                                names(brain$surfaces))
    for (surface_type in missing_surfaces) {
      brain$add_surface(surface_type)
    }
  }, error = function(e) {})

  tryCatch({
    for (atlas in brain_atlas_params(params)) {
      brain_add_atlas(brain, atlas)
    }
  }, error = function(e) {})

  tryCatch({
    annots <- params$annotation_types
    if (is.data.frame(annots) && nrow(annots)) {
      for (ii in seq_len(nrow(annots))) {
        brain$add_annotation(annotation = annots$name[[ii]],
                             surface_type = annots$surface[[ii]])
      }
    }
  }, error = function(e) {})

  tryCatch({
    streamline_types <- brain_type_names(params$streamline_types)
    if (length(streamline_types)) {
      brain$add_streamline(streamline_types)
    }
  }, error = function(e) {})

  invisible(brain)
}

brain_add_atlas <- function(brain, atlas) {
  if (length(atlas$name) != 1 || is.na(atlas$name)) { return(invisible()) }

  brain$add_atlas(
    atlas = atlas$name,
    color_format = atlas$color_format,
    trans_space_from = atlas$trans_space_from
  )
  if (atlas$name %in% names(brain$atlases)) { return(invisible()) }

  # the name no longer resolves against the FreeSurfer folder; re-register the
  # atlas from the cached volume file recorded at write time
  path <- atlas$path
  if (length(path) != 1 || is.na(path) || !file.exists(path)) { return(invisible()) }

  VolumeGeom2 <- call_pkg_fun("threeBrain", "VolumeGeom2", .call_pkg_function = FALSE)
  BrainAtlas <- call_pkg_fun("threeBrain", "BrainAtlas", .call_pkg_function = FALSE)

  atlas_geom <- VolumeGeom2$new(
    name = sprintf("Atlas - %s (%s)", atlas$name, brain$subject_code),
    path = path,
    color_format = atlas$color_format,
    trans_mat = NULL
  )
  atlas_geom$trans_space_from <- atlas$trans_space_from

  atlas_instance <- BrainAtlas$new(
    subject_code = brain$subject_code,
    atlas_type = atlas$name,
    position = c(0, 0, 0),
    atlas = atlas_geom
  )
  atlas_instance$group$.cache_name <- sprintf("%s/mri", brain$subject_code)
  brain$add_atlas(atlas = atlas_instance)

  invisible()
}

brain_restore_electrodes <- function(brain, params) {
  etable <- params$electrode_table
  if (!is.data.frame(etable) || !nrow(etable)) { return(invisible(brain)) }

  # keep the geometry the brain had when it was written: `set_electrodes`
  # promotes contacts to prototypes by default
  priority <- if (isTRUE(params$has_prototypes)) { "prototype" } else { "sphere" }
  brain$set_electrodes(etable, priority = priority)

  vtable <- params$electrode_values
  if (is.data.frame(vtable) && nrow(vtable)) {
    brain$set_electrode_values(vtable)
  }
  invisible(brain)
}


# ---- shape helpers ---------------------------------------------------------

# Names out of any of the shapes a type list has been stored in: a character
# vector, a `data.frame` with a `name` column, or a list of `list(name = )`
brain_type_names <- function(x) {
  if (!length(x)) { return(NULL) }
  if (is.character(x)) { return(unname(x)) }
  if (is.data.frame(x)) { return(as.character(x$name)) }
  if (is.list(x)) {
    nms <- lapply(x, function(item) {
      if (is.list(item)) { item$name } else { as.character(item) }
    })
    return(as.character(unlist(nms)))
  }
  as.character(x)
}

# Atlases as a list of `list(name, color_format, trans_space_from, path)`,
# accepting the `data.frame` shape of `brain$atlas_types` as well
brain_atlas_params <- function(params) {
  atlas_types <- params$atlas_types
  if (!length(atlas_types)) { return(list()) }

  if (is.data.frame(atlas_types)) {
    if (!nrow(atlas_types)) { return(list()) }
    return(lapply(seq_len(nrow(atlas_types)), function(ii) {
      list(
        name = as.character(atlas_types$name[[ii]]),
        color_format = as.character(atlas_types$color_format[[ii]]),
        trans_space_from = as.character(atlas_types$transform_space[[ii]]),
        path = NULL
      )
    }))
  }

  if (is.character(atlas_types)) {
    return(lapply(atlas_types, function(name) { list(name = name) }))
  }

  atlas_types[vapply(atlas_types, is.list, FUN.VALUE = logical(1))]
}

drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


# ---- legacy readers --------------------------------------------------------
# Parameter lists written before `threeBrain_version` existed. Two shapes
# reached disk: `rave_serialize_impl.rave-brain`'s (surfaces as a list of
# `list(name, annotations, vertex_color_types)`) and the `rave-brain` target
# format's (a flat character `surfaces`). Both are read here.

brain_unmarshal_legacy <- function(params, brain = NULL) {
  if (inherits(params, "rave-brain")) { return(params) }
  if (!length(params)) { return(brain) }

  tryCatch({
    if (!inherits(brain, "rave-brain")) {
      project_name <- params$project_name
      if (length(project_name) != 1 || is.na(project_name)) {
        threeBrain <- require_package("threeBrain", return_namespace = TRUE)
        brain <- threeBrain$threeBrain(
          path = params$base_path,
          subject_code = params$subject_code,
          surface_types = "pial",
          atlas_types = NULL
        )
      } else {
        brain <- call_ravecore_fun(
          "rave_brain",
          sprintf("%s/%s", project_name, params$subject_code),
          overlays = NULL,
          usetemplateifmissing = isTRUE(params$usetemplateifmissing),
          include_electrodes = FALSE
        )
      }
    }
    if (!inherits(brain, "rave-brain")) {
      warning("Unable to restore the brain object. Returning `NULL`.")
      return(NULL)
    }

    if (is.list(params$surface_types)) {
      lapply(params$surface_types, function(slist) {
        brain$add_surface(slist$name, vertex_color_types = slist$vertex_color_types)
        lapply(slist$annotations, function(annotation) {
          brain$add_annotation(annotation = annotation, surface_type = slist$name)
          return()
        })
        return()
      })
    }

    # the `rave-brain` target format stored a flat character vector instead
    for (surface_type in c(params$surfaces, brain_type_names(params$surface_types))) {
      if (!surface_type %in% names(brain$surfaces)) {
        brain$add_surface(surface_type)
      }
    }

    for (atlas in brain_atlas_params(params)) {
      brain_add_atlas(brain, atlas)
    }

    brain_restore_electrodes(brain, params)
    brain
  }, error = function(e) {
    warning(brain_import_failure(params, e))
    NULL
  })
}

multibrain_unmarshal_legacy <- function(params) {
  if (inherits(params, "multi-rave-brain")) { return(params) }
  if (!length(params)) { return(NULL) }

  template_params <- params$template_params

  blist <- lapply(params$individual_params, brain_unmarshal_legacy)
  blist <- as.list(blist[!vapply(blist, is.null, FUN.VALUE = logical(1))])

  # the legacy template list stored surfaces under `surfaces`
  merged <- multibrain_merge(
    blist,
    template_subject = template_params$subject_code,
    template_params = list(surface_types = template_params$surfaces)
  )
  if (is.null(merged) || !length(template_params)) { return(merged) }

  template_params <- multibrain_remap_template(template_params, merged$template_object)
  template_object <- brain_unmarshal_legacy(template_params,
                                            brain = merged$template_object)
  if (inherits(template_object, "rave-brain")) {
    merged$template_object <- template_object
  }
  merged
}
