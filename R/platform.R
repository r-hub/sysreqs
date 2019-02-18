
#' Detect the platform R is running on.
#'
#' @section Linux platforms:
#'
#' Linux platforms look like this:
#' ```
#' <r-version>-linux-<arch>-<distribution>-<compiler>
#' ```
#'
#' `r-version` can be a tag: `r-devel`, `r-release` or `r-oldrel`. It
#' may also be a version number.
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
#' r-devel-linux-arm64-debian:9-gcc
#' r-release-linux-x86_64-ubuntu:14.04
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
#' `compiler` can be `factory` or `cran`. If missing, `cran` is assumed.
#' `factory` is the default system compiler. `cran` is the compiler
#' suggested by CRAN.
#'
#' r-release-osx-x86_64
#' r-devel-osx-x86_64
#'
#' @section Other platforms:
#'
#' Other supported platforms:
#' ```
#' <r-version>-solaris-x86-<compiler>
#' ```
#'
#' If `compiler` is missing, Oracle Developer Studio 12.6 is assumed.
#' Otherwise it can be `gcc`.
#'
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

  switch(detect_os(),
    "windows" = detect_platform_windows(),
    "osx"     = detect_platform_osx(),
    "linux"   = detect_platform_linux(),
    "solaris" = detect_platform_solaris(),
    "unknown"
  )
}

detect_platform_windows <- function() {
  rver <- detect_r_version()
  paste(sep = "-", rver, "windows", "ix86+x86_64")
}

detect_platform_osx <- function() {
  rver <- detect_r_version()
  ## TODO: compiler?
  paste(sep = "-", rver, "osx", "x64_86")
}

detect_platform_linux <- function() {
  rver <- detect_r_version()
  arch <- detect_linux_arch()
  dist <- detect_linux_distrib()
  comp <- detect_linux_compiler()
  paste(sep = "-", rver, "linux", arch, dist, comp)
}

detect_platform_solaris <-  function() {
  rver <- detect_r_version()
  ## TODO: compiler?
  paste(sep = "-",  rver, "solaris", "x86")
}

detect_r_version <- function() {

  release <- which_r_version("r-release")
  oldrel <- which_r_version("r-oldrel")
  myself <- my_r_version()

  if (grepl("Under development", R.version$status)) {
    "r-devel"
  } else if (grepl("Patched", R.version$status) && myself == release) {
    "r-patched"
  } else if (R.version$status == "RC") {
    paste0("r-", myself)
  } else if (R.version$status == "" && myself == release) {
    "r-release"
  } else if (R.version$status == "" && myself == oldrel) {
    "r-oldrel"
  } else {
    paste0("r-", myself)
  }
}

which_r_version <- function(str) {
  url <- make_url(rversions_url, version = str)
  download_json(url)[[1]]$version
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
  if (!file.exists("/etc/os-release")) return("unknown")
  lines <- readLines("/etc/os-release")
  dist <- sub("^ID=", "", grep("^ID=", lines, value = TRUE)[1])
  vers <- sub("^VERSION_ID=", "",
              grep("^VERSION_ID=", lines, value = TRUE)[1])
  if (is.na(dist) || is.na(vers)) return ("unknown")
  paste0(unq(dist), ":", unq(vers))
}
