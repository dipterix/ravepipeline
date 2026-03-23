# List Available Help Topics for R Package
docs_package_help_topics <- function(package_name) {
  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    stop("package_name parameter is required and must be a non-empty string")
  }

  # Check if package exists
  if(system.file(package = package_name) == "") {
    stop(sprintf("Package '%s' is not installed or not available", package_name))
  }

  # Get help database for package
  help_db <- utils::help.search(".", package = package_name, rebuild = FALSE)

  if (length(help_db$matches) == 0 || nrow(help_db$matches) == 0) {
    sprintf("Package is installed but no help topics in package '%s'", package_name)
  }

  # Extract topics and titles
  topics <- data.frame(
    topic = as.character(help_db$matches[, "Topic"]),
    title = as.character(help_db$matches[, "Title"]),
    stringsAsFactors = FALSE
  )
  topics <- unique(topics)

  list(
    success = TRUE,
    package = package_name,
    topics = topics,
    count = nrow(topics)
  )
}

# Get R Package Help Page
docs_help_page <- function(topic, package_name = "") {
  # Validate inputs
  if (missing(topic) || !is.character(topic) || length(topic) != 1 || topic == "") {
    stop("`topic` is required and must be a non-empty string")
  }

  if (!is.character(package_name) || length(package_name) != 1) {
    stop("package_name must be a single character string (use empty string to search all packages)")
  }

  if (package_name == "") {
    if(grepl("[:]+", topic)) {
      topic <- strsplit(topic, '[:]+', perl = TRUE)[[1]]
      package_name <- topic[[1]]
      topic <- topic[[2]]
    } else {
      # find from all loaded packages
      db <- utils::help.search(topic, fields = c("alias", "concept", "title"))
      if (!length(db$matches)) {
        stop(sprintf("unable to find the topic `%s`. Please try <package:function> format or pass package argument explicitly", topic))
      }
      package_name <- db$matches$Package[[1]]
      topic <- db$matches$Topic[[1]]
    }
  }

  # Get help file
  args <- list(topic = topic, help_type = "text")
  if (package_name != "") {
    args$package <- package_name
  }
  help_file <- do.call(utils::help, args)

  # Check if help was found
  if (length(help_file) == 0) {
    pkg_msg <- if (package_name == "") {
      "any loaded package"
    } else {
      sprintf("package '%s'", package_name)
    }
    stop(sprintf("No help found for topic '%s' in %s", topic, pkg_msg))
  }

  # Get the resolved help file name from help_file
  # help_file contains the path(s) to the help file
  help_path <- as.character(help_file)
  resolved_name <- basename(help_path)[1]  # Get the first match

  # Get the Rd database for the package
  rd_db <- tools::Rd_db(package_name)

  # Look up the Rd object using the resolved name
  rd_file_name <- paste0(resolved_name, ".Rd")
  rd_obj <- rd_db[[rd_file_name]]

  if (is.null(rd_obj)) {
    stop(sprintf("Could not find help documentation for topic '%s' in package '%s'", topic, package_name))
  }

  # Convert Rd to plain text using tools::Rd2txt (exported function)
  # capture.output returns a character vector (one element per line), so collapse it
  formatted_lines <- utils::capture.output({
    tools::Rd2HTML(rd_obj)
  })
  formatted_content <- paste(formatted_lines, collapse = "\n")

  # Extract package name from help_file attributes or use provided package_name
  actual_package <- if (package_name == "") {
    # Try to extract from help_file structure
    pkg_info <- attr(help_file, "topic")
    if (!is.null(pkg_info) && length(pkg_info) > 0) {
      pkg_info[1]
    } else {
      "unknown"
    }
  } else {
    package_name
  }

  list(
    success = TRUE,
    topic = topic,
    package = actual_package,
    content = formatted_content
  )
}

# Print package object with implementation details
docs_package_implementation <- function(fun_name, package_name = "") {
  # Validate inputs
  if (missing(fun_name) || !is.character(fun_name) || length(fun_name) != 1 || fun_name == "") {
    stop("`fun_name` is required and must be a non-empty string")
  }

  if (!is.character(package_name) || length(package_name) != 1) {
    stop("`package_name` must be a single character string (use empty string only if `fun_name` is in `pkg::fun` form)")
  }

  if (!grepl(":", fun_name)) {
    if (!nzchar(package_name)) {
      stop("`fun_name` can either be explicit with `pkg` with `pkg::fun` or you must specify `package_name`")
    }
  } else {
    fun_name <- strsplit(fun_name, "[:]+", perl = TRUE)[[1]]
    package_name <- fun_name[[1]]
    fun_name <- fun_name[[2]]
  }

  if (system.file(package = package_name) == "") {
    stop(sprintf("Package `%s` is not installed", package_name))
  }

  ns <- asNamespace(package_name)
  impl <- ns[[fun_name]]

  impl_str <- utils::capture.output({
    print(impl)
  })

  impl_str <- paste(impl_str, collapse = "\n")

  return(impl_str)

}


# Get R Package Information
search_package_info <- function(package_name) {
  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    stop("package_name parameter is required and must be a non-empty string")
  }

  # Check if package exists
  if (system.file(package = package_name) == "") {
    stop(sprintf("Package '%s' is not installed or not available", package_name))
  }

  # Get package description
  pkg_desc <- utils::packageDescription(package_name)

  if (is.null(pkg_desc) || inherits(pkg_desc, "try-error")) {
    stop(sprintf("Could not retrieve information for package '%s'", package_name))
  }

  # Extract key fields (convert to list, handling NULL values)
  info <- list(
    package = package_name,
    version = pkg_desc$Version %||% NA_character_,
    title = pkg_desc$Title %||% NA_character_,
    description = pkg_desc$Description %||% NA_character_,
    author = pkg_desc$Author %||% NA_character_,
    maintainer = pkg_desc$Maintainer %||% NA_character_,
    license = pkg_desc$License %||% NA_character_,
    depends = pkg_desc$Depends %||% NA_character_,
    imports = pkg_desc$Imports %||% NA_character_,
    suggests = pkg_desc$Suggests %||% NA_character_,
    url = pkg_desc$URL %||% NA_character_,
    bugreports = pkg_desc$BugReports %||% NA_character_,
    date = pkg_desc$Date %||% NA_character_,
    repository = pkg_desc$Repository %||% NA_character_,
    built = pkg_desc$Built %||% NA_character_
  )

  info
}

