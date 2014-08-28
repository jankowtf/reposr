context("package")

test_that("ensureRepositoryInfrastructure", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  repos_home <- file.path(tempdir(), "repos_home")
  dir.create(repos_home, recursive=TRUE, showWarnings=FALSE)
  repos_home <- normalizePath(repos_home, winslash="/", mustWork=FALSE)

  expect_true(all(res <- ensureRepositoryInfrastructure(repos_home = repos_home)))
  expect_true(length(res) == 2)
  expect_true(all(res <- ensureRepositoryInfrastructure(
    repos_home = repos_home, pkg_name = character(), pkg_version = character())))
  expect_true(length(res) == 1)
  expect_true(all(res <- ensureRepositoryInfrastructure(
    repos_home = repos_home, pkg_name = "test", pkg_version = "1.0")))
  expect_true(length(res) == 1)
  
  on.exit(
  {
    if (grepl(basename(tempdir()), repos_home)) {
      unlink(repos_home, recursive=TRUE, force=TRUE)
    }
  }
    )
  
  }
)

