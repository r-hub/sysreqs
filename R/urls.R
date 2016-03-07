
sysreqs_url <- "http://sysreqs.r-hub.io/map-platform/:platform/:query"
sysreqs_cran_url <- "http://sysreqs.r-hub.io/pkg/:package/:platform"
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
