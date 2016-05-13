
#' Get the system requirements of a CRAN package
#'
#' @param package \assert{is_package_name} Package name.
#' @param platform Platform string. Defaults to the current platform.
#' @return All system dependencies on the current or the specified
#'   platform.
#'
#' @keywords internal

get_cran_sysreqs <- function(package, platform = current_platform()) {

  url <- make_url(sysreqs_cran_url, package = package, platform = platform)

  unlist(download_json(url))
}


#' System requirements of a local package
#'
#' @param desc Path to a \code{DESCRIPTION} file.
#' @param platform Platform string, defaults to the current platform.
#' @return All system dependencies on the current or the specified
#'   platform.
#'
#' @export
#' @importFrom desc description

sysreqs <- function(desc, platform = current_platform()) {
  dsc <- description$new(desc)

  sysreqs_field <- dsc$get("SystemRequirements")
  own_sysreqs <- if (is.na(sysreqs_field)) {
    character()
  } else {
    get_sysreqs(sysreqs_field, platform = platform)
  }

  deps <- dsc$get_deps()

  ## We include the package itself, because it might have an override
  all_deps <- get_cran_deps(c(dsc$get("Package"), deps$package))

  dep_sysreqs <- lapply(all_deps, get_cran_sysreqs, platform = platform)

  c(own_sysreqs, do.call(c, dep_sysreqs)) %||% character()
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
