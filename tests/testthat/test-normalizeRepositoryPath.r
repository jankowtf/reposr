context("package")

test_that("normalizeRepositoryPath", {

  repos  <- file.path(tempdir(), "repos")
  repos  <- normalizePath(repos, winslash = "/", mustWork = FALSE)
  expected <- repos
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos), expected)
  expected <- paste0("file:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_file"), expected)
  expect_equivalent(res <- normalizeRepositoryPath(repos = res, 
      type = "url_file"), expected)
  expected <- repos
  expect_equivalent(res <- normalizeRepositoryPath(repos = res), expected)

  expected <- paste0("http:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_http"), expected)
  
  expected <- paste0("ftp:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_ftp"), expected)
  
  ## Condition handling //
  expect_error(normalizeRepositoryPath(repos = repos, 
      type = "nonexistingtype"))
  
  }
)

