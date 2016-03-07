
#' @export

current_platform <- function() {
  env_var <- Sys.getenv("RHUB_PLATFORM")
  if (! identical(env_var, "")) {
    env_var
  } else {
    detect_platform()
  }
}

## Tries to reliably detect CRAN platforms for now
## 2016-02-26 16:49 GMT
##
## r-devel-linux-x86_64-debian-gcc
## r-devel-linux-x86_64-fedora-clang
## r-devel-linux-x86_64-fedora-gcc
## r-devel-osx-x86_64-clang
## r-devel-windows-ix86+x86_64
## r-patched-linux-x86_64
## r-patched-solaris-sparc
## r-patched-solaris-x86
## r-release-linux-x86_64
## r-release-osx-x86_64-mavericks
## r-release-windows-ix86+x86_64
## r-oldrel-windows-ix86+x86_64

#' @export

detect_platform <- function() {

  rver <- detect_r_version()
  os   <- detect_os()
  comp <- detect_compiler()
  arch <- detect_arch(os)
  pkgtype <- detect_pkgtype(os)

  platform <- paste(
    sep = "-",
    rver,
    os,
    arch
  )

  ## r-devel-linux-x86_64-debian-gcc
  ## r-devel-linux-x86_64-fedora-clang
  ## r-devel-linux-x86_64-fedora-gcc
  if (rver == "r-devel" && os == "linux") {
    platform <- paste(
      sep = "-",
      platform,
      detect_linux_distrib(),
      comp
    )
  }

  ## r-devel-osx-x86_64-clang
  if (rver == "r-devel" && os == "osx") {
    platform <- paste(platform, sep = "-", comp)
  }

  ## r-release-osx-x86_64-mavericks
  ## I am not completely sure what this is, btw.
  if (rver == "r-release" && os == "osx" && pkgtype == "mavericks") {
    platform <- paste(platform, sep = "-", "mavericks")
  }

  platform
}

## This is dummy, not a real detection

detect_arch <- function(os = detect_os()) {
  if (os == "linux") {
    "x86_64"
  } else if (os == "windows") {
    "ix86+x86_64"
  } else if (os == "osx") {
    "x86_64"
  } else if (os == "solaris") {
    .Platform$r_arch
  } else {
    NA_character_
  }
}

## Again, this is a dummy
detect_pkgtype <- function(os = detect_os()) {
  if (os == "osx") {
    if (grepl("mavericks", .Platform$pkgType)) "mavericks" else ""
  } else {
    ""
  }
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
    NA_character_
  } else if (R.version$status == "" && myself == release) {
    "r-release"
  } else if (R.version$status == "" && myself == oldrel) {
    "r-oldrel"
  } else {
    NA_character_
  }
}

which_r_version <- function(str) {
  url <- make_url(rversions_url, version = str)
  download(url)[[1]]$version
}

my_r_version_string <- function(str) {
  paste(R.version$major, sep = ".", R.version$minor)
}

my_r_version <- function() {
  paste(R.version$major, sep = ".", R.version$minor)
}

detect_os <- function() {

  ostype <- os_type()
  sysname <- Sys.info()["sysname"]
  if (ostype == "win") {
    "windows"
  } else if (sysname == "Darwin") {
    "osx"
  } else if (sysname == "Linux") {
    "linux"
  } else if (sysname == "Solaris") {
    "solaris"
  } else {
    stop("sysreqs is not supported on your platform")
  }
}

detect_compiler <- function(os = detect_os()) {
  if (os %in% c("windows", "solaris")) {
    ""
  } else {
    Makeconf <- readLines(file.path(R.home("etc"), "Makeconf"))
    ccline <- grep("^CC[ ]?=", Makeconf, value = TRUE)
    sub("^CC[ ]?=[ ]?", "", ccline)
  }
}

detect_linux_distrib <- function(os = detect_os()) {
  if (os != "linux") return(NA_character_)

  if (file.exists("/etc/lsb-release") &&
      grepl("DISTRIB ID=Ubuntu", readLines("/etc/lsb-release"))) {
    "ubuntu"
  } else if (file.exists("/etc/debian_release")) {
    "debian"
  } else if (file.exists("/etc/fedora-release")) {
    "fedora"
  } else {
    NA_character_
  }
}
