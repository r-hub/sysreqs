
#' Command(s) to install system requirements of a package, on a platform
#'
#' @inheritParams sysreqs
#' @return Character scalar, a shell command to run to install
#'   the system requirements on a given platform.
#'
#' @export

sysreq_commands <- function(desc, platform = current_platform()) {

  pkgs <- paste(sysreqs(desc, platform), collapse = " ")

  url <- make_url(
    sysreqs_platform_url,
    platform = platform
  )

  cmd <- download_json(url)[["install-commands"]]

  sub("${sysreqs}", pkgs, cmd, fixed = TRUE)
}
