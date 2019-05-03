
#' Get the system requirements of a CRAN package
#'
#' @param package Package name.
#' @param platform Platform string. Defaults to the current platform.
#' @return All system dependencies on the current or the specified
#'   platform.
#'
#' @keywords internal

get_cran_sysreqs <- function(package, platform = current_platform()) {

  if (!length(package)) return(character())

  package <- paste(package, collapse = ",")
  url <- make_url(sysreqs_cran_url, package = package, platform = platform)

  unlist(download_json(url))
}


#' System requirements of a local package
#'
#' @param desc Path to a \code{DESCRIPTION} file.
#' @param platform Platform string, defaults to the current platform.
#' @param soft Whether to include soft dependencies.
#' @return All system dependencies on the current or the specified
#'   platform.
#'
#' @export
#' @importFrom desc description

sysreqs <- function(desc, platform = current_platform(), soft = TRUE) {
  dsc <- description$new(desc)

  sysreqs_field <- dsc$get("SystemRequirements")
  own_sysreqs <- if (is.na(sysreqs_field)) {
    character()
  } else {
    get_sysreqs(sysreqs_field, platform = platform)
  }

  deps <- dsc$get_deps()

  ## Remove soft dependencies if they are not wanted
  if (!soft) {
    deps <- deps[ deps$type %in% c("Depends", "Imports", "LinkingTo"),, drop = FALSE ]
  }

  ## We include the package itself, because it might have an override
  all_deps <- get_cran_deps(c(dsc$get("Package"), deps$package))

  ## Get all sysreqs at once
  dep_sysreqs <- get_cran_sysreqs(all_deps, platform = platform)

  c(own_sysreqs, dep_sysreqs) %||% character()
}


get_sysreqs <- function(query, platform = current_platform()) {

  query <- as.character(query)[1]
  url <- make_url(
    sysreqs_url,
    query = encode_slash(URLencode(query)),
    platform = platform
  )

  unlist(download_json(url))
}
