
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
  }

  db <- utils::available.packages()

  deps <- tools::package_dependencies(
    packages,
    recursive = TRUE,
    which = c("Depends", "Imports", "LinkingTo"),
    db = db
  )

  # Get rid of base packages
  deps <- unique(c(packages, unlist(deps, use.names = FALSE)))
  base <- rownames(installed.packages(priority = "base"))
  deps <- setdiff(deps, base)

  deps
}
