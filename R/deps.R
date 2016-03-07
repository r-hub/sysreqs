
#' Recursive (hard) dependencies of CRAN packages
#'
#' @param packages Character vector of package names.
#' @return A character vector of package names and versions,
#'   separated by a single dash.
#'
#' @keywords internal

get_cran_deps <- function(packages) {

  if (length(packages) == 0) {
    character()

  } else {
    url <- make_url(crandeps_url, packages = paste(packages, collapse = ","))
    pkgs <- names(download_json(url))

    ## The base packages have no dash
    grep("-", pkgs, fixed = TRUE, value = TRUE)
  }
}
