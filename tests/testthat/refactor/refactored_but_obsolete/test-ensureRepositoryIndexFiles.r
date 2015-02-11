##------------------------------------------------------------------------------
context("ensureRepositoryIndexFiles")
##------------------------------------------------------------------------------

.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos_0 <- file.path(tempdir(), "repos")
.cleanTempDir(repos_0)
repos <- asRepository(repos = repos_0)
sapply(repos$sublevel, dir.create, recursive = TRUE, showWarnings = FALSE)

test_that("ensureRepositoryIndexFiles/character", {

  nms <- repos$sublevel
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndexFiles(
    repos = repos_0), expected)

})

test_that("ensureRepositoryIndexFiles/PackageRepositoryRoot.S3", {
  
  nms <- repos
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndexFiles(
    repos = asRepositoryRoot(repos = repos)), expected)
  
})

test_that("ensureRepositoryIndexFiles/PackageRepositoryRoot.S3", {

  nms <- repos
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndexFiles(repos = repos), expected)
  
})

## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3
## PackageRepositorySubWindows.S3

test_that("ensureRepositoryIndexFiles/sublevels", {
  
  sapply(seq(along=repos), function(ii) {
    expect_equivalent(res <- ensureRepositoryIndexFiles(repos = repos[[ii]]), expected[[ii]])
  })
  
})

test_that("ensureRepositoryIndexFiles/conditions", {
  
  .cleanTempDir(x = repos_0)
  expect_error(res <- ensureRepositoryIndexFiles(repos = repos))
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

