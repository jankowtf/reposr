context("buildIntoRepositoryInfrastructure-A")

test_that("buildIntoRepositoryInfrastructure", {

  if (basename(getwd()) == "testthat") {
    path_0 <- "data"
  } else {
    path_0 <- "tests/testthat/data"
  }
  
  path_pkg <- file.path(path_0, "test.package")
  if (FALSE) {
  ## Needs more manual adjustments than just running 'package.skeleton()' //
    if (!file.exists(path_pkg)) {
      package.skeleton(name = "test.package", path = path_0)  
    }
  }
  
  wd_0 <- setwd(path_pkg)
  
  repos_home <- file.path(tempdir(), "repos")
  dir.create(repos_home, recursive = TRUE, showWarnings = FALSE)
  repos_home <- normalizePath(repos_home, winslash="/", mustWork = TRUE)

  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home)))
  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home, binary = TRUE)))
  expect_true(length(res) == 2)
  expect_true(all(sapply(names(res), file.exists)))
  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home, pkg_name = character(), pkg_version = character())))
  expect_true(length(res) == 2)
  
  on.exit({
    if (grepl(basename(tempdir()), repos_home)) {
      unlink(repos_home, recursive=TRUE, force=TRUE)
    }
  })
  
})

