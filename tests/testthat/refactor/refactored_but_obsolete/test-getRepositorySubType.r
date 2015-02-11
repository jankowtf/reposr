##------------------------------------------------------------------------------
context("getRepositorySubType")
##------------------------------------------------------------------------------

repos_0 <- "c:"
repos <- asRepository(repos = repos_0)
expected <- sort(c("mac.binary", "win.binary", "source"))
names(expected) <- repos$sublevel

test_that("getRepositorySubType/character", {
  
  expect_equivalent(res <- getRepositorySubType(repos = repos_0), expected)

})

test_that("getRepositorySubType/PackageRepositoryRoot.S3", {
  
  expect_equivalent(res <- getRepositorySubType(
    repos = asRepositoryRoot(repos_0)), expected)

})

test_that("getRepositorySubType/PackageRepository.S3", {
  
  expect_equivalent(res <- getRepositorySubType(repos = repos), expected)
  
})


## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3
## PackageRepositorySubWindows.S3

test_that("getRepositorySubType/sublevel", {
  
  sapply(seq(along=repos$sublevel), function(ii) {
    expect_equivalent(
      res <- getRepositorySubType(repos=repos$sublevel[[ii]]), expected[[ii]])
  })
  
})
