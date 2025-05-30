% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\name{ravepipeline_finalize_installation}
\alias{ravepipeline_finalize_installation}
\title{Download 'RAVE' built-in pipelines and code snippets}
\usage{
ravepipeline_finalize_installation(
  upgrade = c("ask", "always", "never", "config-only", "data-only"),
  async = FALSE,
  ...
)
}
\arguments{
\item{upgrade}{rules to upgrade dependencies; default is to ask if needed}

\item{async}{whether to run in the background; ignore for now}

\item{...}{ignored; reserved for external calls.}
}
\value{
A list built-in pipelines will be installed, the function itself
returns nothing.
}
\description{
The official built-in pipeline repository is located at
\url{https://github.com/rave-ieeg/rave-pipelines};
The code snippet repository is located at
\url{https://github.com/rave-ieeg/rave-gists}.
}
\examples{

\dontrun{

# This function requires connection to the Github, and must run
# under interactive session since an user prompt will be displayed

ravepipeline_finalize_installation()

}

}
