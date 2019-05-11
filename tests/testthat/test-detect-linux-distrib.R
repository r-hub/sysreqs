
context("detect_linux_distrib")

test_that("detect_linux_distrib", {
  skip_on_cran()
  skip_without_docker()

  cases <- list(
    c("debian:8",       "debian:8"),
    c("debian:9",       "debian:9"),
    c("debian:testing", "debian:unstable"),
    c("ubuntu:14.04",   "ubuntu:14.04"),
    c("ubuntu:16.04",   "ubuntu:16.04"),
    c("ubuntu:18.04",   "ubuntu:18.04"),
    c("ubuntu:18.10",   "ubuntu:18.10"),
    c("ubuntu:19.04",   "ubuntu:19.04"),
    c("fedora:27",      "fedora:27"),
    c("fedora:28",      "fedora:28"),
    c("fedora:29",      "fedora:29"),
    c("fedora:30",      "fedora:30"),
    c("fedora:31",      "fedora:31"),
    c("centos:6",       "centos:6"),
    c("centos:7",       "centos:7"),
    c("archlinux/base", "arch"),
    c("opensuse/leap:15.0",  "opensuse-leap:15"),
    c("opensuse/leap:42.3",  "opensuse:42")
  )

  linux_sh <- system.file("scripts", "linux.sh", package = "sysreqs")

  for (case in cases) {
    out <- call_on_docker(case[1], linux_sh)
    expect_equal(out$stdout, paste0(case[2], "\n"), info = case[1])
  }
})
