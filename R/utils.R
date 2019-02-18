
os_type <- function() {
  .Platform$OS.type
}

`%||%` <- function(l, r) { if (is.null(l)) r else l }

encode_slash <- function(x) {
  gsub("/", "&#47;", x, fixed = TRUE)
}

str_trim <- function(x) {
  sub(
    "^\\s+",
    "",
    sub("\\s+$", "", x, perl = TRUE),
    perl = TRUE
  )
}

unq <- function(x) {
  l <- nchar(x)
  q <-
    (substr(x, 1, 1) == '"' & substr(x, l, l) == '"') |
    (substr(x, 1, 1) == "'" & substr(x, l, l) == "'")
  if (any(q)) x[q] <- substr(x, 2, l - 1)
  x
}
