
download <- function(url, quiet = TRUE) {

  "!DEBUG Downloading `url`"
  path <- tempfile()

  status <- utils::download.file(
    url,
    path,
    method = download_method(),
    quiet = quiet,
    mode = "wb"
  )

  if (status != 0)  stop("Cannot download file from ", url, call. = FALSE)

  path
}

download_method <- function() {

  if (isTRUE(unname(capabilities("libcurl")))) {
    "libcurl"

  } else if (os_type() == "windows") {
    "wininet"

  } else {
    "auto"
  }
}

download_json <- function(url, quiet = TRUE) {

  path <- download(url, quiet = quiet)

  content <- jsonlite::fromJSON(path)

  unlink(path)

  content
}
