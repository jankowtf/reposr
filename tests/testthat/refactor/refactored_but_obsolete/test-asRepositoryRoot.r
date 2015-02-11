.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos_0  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))
dir.create(repos_0, showWarnings = FALSE)
repos_0_url <- file.path("file://", repos_0)

##------------------------------------------------------------------------------
context("asRepositoryRoot/character")
##------------------------------------------------------------------------------

test_that("asRepositoryRoot/character", {

  expected <- "PackageRepositoryRoot.S3"
  expect_is(res <- asRepositoryRoot(repos = repos_0), expected)
  expect_is(res <- asRepositoryRoot(repos = repos_0_url), expected)
  expect_is(res <- asRepositoryRoot(repos = repos_0, type = "url_fs"), expected)

})

##------------------------------------------------------------------------------
context("asRepositoryRoot/PackageRepositoryRoot.S3")
##------------------------------------------------------------------------------

test_that("asRepositoryRoot/PackageRepositoryRoot.S3", {
  
  expect_is(res <- asRepositoryRoot(repos = asRepositoryRoot(repos_0)), expected)
  expect_is(res <- asRepositoryRoot(repos = asRepositoryRoot(repos_0_url)), expected)
  expect_is(res <- asRepositoryRoot(repos = asRepositoryRoot(repos_0), 
    type = "url_fs"), expected)

})

##------------------------------------------------------------------------------
context("asRepositoryRoot/PackageRepository.S3")
##------------------------------------------------------------------------------

test_that("asRepositoryRoot/PackageRepository.S3", {
  
  expect_is(res <- asRepositoryRoot(repos = asRepository(repos_0)), expected)
  expect_is(res <- asRepositoryRoot(repos = asRepository(repos_0_url)), expected)
  expect_is(res <- asRepositoryRoot(repos = asRepository(repos_0),
      type = "url_fs"), expected)

})

##------------------------------------------------------------------------------
context("asRepositoryRoot/branches")
##------------------------------------------------------------------------------

## PackageRepositorySubMac.S3
## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3

test_that("asRepositoryRoot/branches", {
  
  repos <- asRepository(repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_is(res <- asRepositoryRoot(repos = repos[[ii]]), expected)
  })
  repos <- asRepository(repos_0, type = "url_fs")
  sapply(seq(along=repos), function(ii) {
    expect_is(res <- asRepositoryRoot(repos = repos[[ii]]), expected)
  })
  
})

##------------------------------------------------------------------------------
context("asRepositoryRoot/ensure")
##------------------------------------------------------------------------------

test_that("asRepositoryRoot/ensure", {
  
  expect_true(file.exists(asRepositoryRoot(repos = repos_0, ensure = TRUE)))
  .cleanTempDir(x = repos_0)
  expect_true(file.exists(asRepositoryRoot(repos = repos_0_url, ensure = TRUE)))
  .cleanTempDir(x = repos_0)
  
  expect_true(file.exists(
    asRepositoryRoot(repos = asRepositoryRoot(repos_0), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  expect_true(file.exists(
    asRepositoryRoot(repos = asRepositoryRoot(repos_0_url), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  
  expect_true(file.exists(
    asRepositoryRoot(repos = asRepository(repos_0), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  expect_true(file.exists(
    asRepositoryRoot(repos = asRepository(repos_0_url), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

