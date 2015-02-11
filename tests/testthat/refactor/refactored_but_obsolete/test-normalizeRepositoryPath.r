.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos  <- file.path(tempdir(), "repos")
dir.create(repos, showWarnings = FALSE)
repos  <- normalizePath(repos, winslash = "/", mustWork = FALSE)

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/fs")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/fs", {

  expected <- repos
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos), expected)
  
})

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/url_fs")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/url_fs", {
  
  expected <- paste0("file:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_fs"), expected)
  expect_equivalent(res <- normalizeRepositoryPath(repos = res, 
      type = "url_fs"), expected)
  
})

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/url_http")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/url_http", {
  
  expected <- repos
  expect_equivalent(res <- normalizeRepositoryPath(repos = res), expected)

  expected <- paste0("http:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_http"), expected)
})

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/url_ftp")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/url_ftp", {
  
  expected <- paste0("ftp:///", repos)
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos, 
      type = "url_ftp"), expected)
  
})

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/conditions")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/conditions", {
  
  expect_error(normalizeRepositoryPath(repos = repos, 
      type = "nonexistingtype"))
  
})

##------------------------------------------------------------------------------
context("normalizeRepositoryPath/PackageRepositoryRoot.S3")
##------------------------------------------------------------------------------

test_that("normalizeRepositoryPath/PackageRepositoryRoot.S3", {
  
  repos_2 <- PackageRepositoryRoot.S3(path = repos)
  expected <- repos_2
  expect_equivalent(res <- normalizeRepositoryPath(repos = repos_2), expected)
  
})
