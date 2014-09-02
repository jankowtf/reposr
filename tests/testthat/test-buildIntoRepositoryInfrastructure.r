context("package")

test_that("buildIntoRepositoryInfrastructure", {

  repos_home <- file.path(tempdir(), "repos")
  dir.create(repos_home, recursive = TRUE, showWarnings = FALSE)
  repos_home <- normalizePath(repos_home, winslash="/", mustWork = TRUE)

  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home)))
  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home, binary = TRUE)))
  expect_true(length(res) == 2)
  expect_true(all(res <- buildIntoRepositoryInfrastructure(
    repos_home = repos_home, pkg_name = character(), pkg_version = character())))
  expect_true(length(res) == 2)
  
  on.exit(
  {
    if (grepl(basename(tempdir()), repos_home)) {
      unlink(repos_home, recursive=TRUE, force=TRUE)
    }
  }
    )
  
  }
)

