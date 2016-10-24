
#' Command(s) to install system requirements of a package, on a platform
#'
#' @inheritParams sysreqs
#' @return Character scalar, a shell command to run to install
#'   the system requirements on a given platform.
#'
#' @export

sysreq_commands <- function(desc, platform = current_platform()) {

  pkgs <- sysreqs(desc, platform)

  url <- make_url(
    sysreqs_platform_url,
    platform = platform
  )

  cmd <- download_json(url)[["install-commands"]]

  pkgs <- unique(pkgs)

  scripts <- grep("^script: ", pkgs, value = TRUE)
  pkgs <- setdiff(pkgs, scripts)

  script_inst <- if (length(scripts)) {
    files <- gsub("^script: ", "", scripts)
    paste0(
      sprintf("bash <(curl -L -s %s/script/%s)", sysreqs_base_url, files),
      collapse = "\n"
    )
  }

  pkg_inst <- if (length(pkgs)) {
    pkgs_str <- paste(pkgs, collapse = " ")
    sub("${sysreqs}", pkgs_str, cmd, fixed = TRUE)
  }

  if (length(script_inst) + length(pkg_inst) > 0) {
    str_trim(paste(pkg_inst, script_inst, sep = "\n"))
  }
}
