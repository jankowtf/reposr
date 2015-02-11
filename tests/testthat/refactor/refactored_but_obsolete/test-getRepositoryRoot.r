.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos_0  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))
dir.create(repos_0, showWarnings = FALSE)

##------------------------------------------------------------------------------
context("getRepositoryRoot")
##------------------------------------------------------------------------------

test_that("getRepositoryRoot/missing", {

  repos <- repos_0
  
  expected <- PackageRepositoryRoot.S3(path = normalizePath(".", winslash = "/"))
  expect_equal(res <- getRepositoryRoot(), expected)
  
})

test_that("getRepositoryRoot/character", {
  
  expected <- PackageRepositoryRoot.S3(path = normalizePath(repos_0, 
    winslash = "/"))
  expect_equal(res <- getRepositoryRoot(repos = repos), expected)
  
})

test_that("getRepositoryRoot/PackageRepositoryRoot.S3", {
  
  repos <- PackageRepositoryRoot.S3(
    path = normalizePath(repos_0, winslash = "/"))
  expected <- repos
  expect_equal(res <- getRepositoryRoot(repos = repos), expected)

})

test_that("getRepositoryRoot/PackageRepository.S3", {
  
  expected <- asRepositoryRoot(repos = repos_0)
  expect_equal(res <- getRepositoryRoot(repos = asRepository(repos_0)), expected)

})

## PackageRepositorySubMac.S3
## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3

test_that("getRepositoryRoot/sublevel", {
  
  repos <- asRepository(repos = repos_0)
  expected <- asRepositoryRoot(repos = repos_0)
  sapply(seq(along = repos$sublevel), function(ii) {
    expect_equal(res <- getRepositoryRoot(repos = repos$sublevel[[ii]]), expected)
  })
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

