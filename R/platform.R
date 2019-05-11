
#' Detect the platform R is running on.
#'
#' @section Linux platforms:
#'
#' Linux platforms look like this:
#' ```
#' <r-version>-linux-<arch>-<distribution>-<compiler>
#' ```
#'
#' The `<r-version>` part is optional and it is currently ignored for
#' matching platforms. It is allowed because CRAN platform names have it.
#'
#' If `<distribution>` and `<compiler>` are missing, then `debian` and
#' `gcc` are assumed. If the compiler is missing, `gcc` is assumed.
#'
#' If `distribution` is `debian` that means Debian testing. If it is
#' `fedora`, the CRAN Fedora version is assumed, see at
#' <https://cran.r-project.org/web/checks/check_flavors.html>.
#'
#' CRAN Linux platforms:
#' ```
#' r-devel-linux-x86_64-debian-clang
#' r-devel-linux-x86_64-debian-gcc
#' r-devel-linux-x86_64-fedora-clang
#' r-devel-linux-x86_64-fedora-gcc
#' r-release-linux-x86_64
#' r-patched-linux-x86_64
#' ```
#'
#' Other examples:
#' ```
#' linux-arm64-debian:9-gcc
#' linux-x86_64-ubuntu:14.04
#' r-3.5.2-linux-x86_64-fedora:28
#' ```
#'
#' @section Windows platforms:
#'
#' Windows platforms are simpler, since binary compatibility is better
#' on Windows:
#' ```
#' <r-version>-windows-<archs>-<rtools-version>
#' ```
#'
#' Again, `<r-version>` is optional and it is ignored.
#'
#' If `rtools-version` is missing, the default for the given R version
#' is assumed.
#'
#' CRAN Windows platforms:
#' ```
#' r-devel-windows-ix86+x86_64
#' r-release-windows-ix86+x86_64
#' r-oldrel-windows-ix86+x86_64
#' ```
#'
#' @section macOS platforms:
#'
#' ```
#' <r-version>-osx-x86_64-<compiler>
#' ```
#'
#' Again, `<r-version>` is optional and it is ignored.
#'
#' `compiler` can be `factory` or `cran`. If missing, `cran` is assumed.
#' `factory` is the default system compiler. `cran` is the compiler
#' suggested by CRAN.
#'
#' CRAN macOS platforms:
#' ```
#' r-release-osx-x86_64
#' r-devel-osx-x86_64
#' ```
#'
#' @section Other platforms:
#'
#' Other supported platforms:
#' ```
#' <r-version>-solaris-x86-<compiler>
#' ```
#'
#' Again, `<r-version>` is optional and it is ignored.
#'
#' If `compiler` is missing, Oracle Developer Studio 12.6 is assumed.
#' Otherwise it can be `gcc`.
#'
#' @return Character scalar, platform string.
#'
#' @export

current_platform <- function() {
  env_var <- Sys.getenv("RHUB_PLATFORM")
  if (! identical(env_var, "")) {
    env_var
  } else {
    detect_platform()
  }
}

detect_platform <- function() {

  rver <- paste0("r-", my_r_version())

  plat <- switch(detect_os(),
    "windows" = detect_platform_windows(),
    "osx"     = detect_platform_osx(),
    "linux"   = detect_platform_linux(),
    "solaris" = detect_platform_solaris(),
    "unknown"
  )

  paste(sep = "-", rver, plat)
}

detect_platform_windows <- function() {
  paste(sep = "-", "windows", "ix86+x86_64")
}

detect_platform_osx <- function() {
  ## TODO: compiler?
  paste(sep = "-", "osx", "x64_86")
}

detect_platform_linux <- function() {
  arch <- detect_linux_arch()
  dist <- detect_linux_distrib()
  comp <- detect_linux_compiler()
  paste(sep = "-", "linux", arch, dist, comp)
}

detect_platform_solaris <-  function() {
  ## TODO: compiler?
  paste(sep = "-", "solaris", "x86")
}

my_r_version <- function() {
  paste(R.version$major, sep = ".", R.version$minor)
}

detect_os <- function() {

  ostype <- os_type()
  sysname <- Sys.info()["sysname"]
  if (ostype == "windows") {
    "windows"
  } else if (sysname == "Darwin") {
    "osx"
  } else if (sysname == "Linux") {
    "linux"
  } else if (sysname == "Solaris") {
    "solaris"
  } else {
    "unknown"
  }
}

detect_linux_arch <- function() {
  R.version$arch
}

detect_linux_compiler <- function() {
  Makeconf <- readLines(file.path(R.home("etc"), "Makeconf"))
  ccline <- grep("^CC[ ]?=", Makeconf, value = TRUE)
  cc <- sub("^CC[ ]?=[ ]?", "", ccline)
  basename(strsplit(cc, " ", fixed = TRUE)[[1]][1])
}

detect_linux_distrib <- function() {
  linux_sh <- system.file("scripts", "linux.sh", package = "sysreqs")
  str_trim(processx::run(linux_sh)$stdout)
}
