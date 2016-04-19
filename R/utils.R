
os_type <- function() {
  .Platform$OS.type
}

`%||%` <- function(l, r) { if (is.null(l)) r else l }

encode_slash <- function(x) {
  gsub("/", "&#47;", x, fixed = TRUE)
}
