
skip_without_docker <- function() {
  if (!nzchar(Sys.getenv("R_SYSREQS_DOCKER_TESTS"))) skip("Needs Docker")
  if (!nzchar(Sys.which("docker"))) skip("No Docker found")
}

test_temp_file <- function(fileext = "", pattern = "test-file-",
                           envir = parent.frame(), create = TRUE) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  if (identical(envir, .GlobalEnv)) {
    message("Temporary files will _not_ be cleaned up")
  } else {
    withr::defer(
      try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir)
  }
  if (create) {
    cat("", file = tmp)
    normalizePath(tmp)
  } else {
    tmp
  }
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern = pattern, envir = envir, create = FALSE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  normalizePath(tmp)
}

call_on_docker <- function(image, script_path) {
  script_file <- basename(script_path)
  tmp_file <- paste0("/tmp/", script_file)
  processx::run(
    "docker",
    c("run", "-i",
      "-v", paste0(script_path, ":", tmp_file),
      image, tmp_file))
}
