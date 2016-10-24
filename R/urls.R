
sysreqs_base_url <- "http://sysreqs.r-hub.io"
## sysreqs_base_url <- "http://localhost:3000" # for local testing
sysreqs_url <- paste0(sysreqs_base_url, "/map-platform/:platform/:query")
sysreqs_cran_url <- paste0(sysreqs_base_url, "/pkg/:package/:platform")
sysreqs_platform_url <- paste0(sysreqs_base_url, "/platform/get/:platform")
crandeps_url <- "http://crandeps.r-pkg.org/deps/:packages"
rversions_url <- "http://rversions.r-pkg.org/:version"

make_url <- function(url, ...) {
  params <- list(...)
  names <- names(params)

  for (i in seq_along(params)) {
    p <- params[[i]][1]
    url <- gsub(paste0(":", names[i], "\\b"), params[[i]][1], url)
  }

  url
}
