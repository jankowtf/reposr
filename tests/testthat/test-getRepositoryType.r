context("package")

test_that("getRepositoryType", {

  repos_0 <- "c:"
  repos <- asExpandedRepository(repos = repos_0)
  expected <- sort(c("mac.binary", "win.binary", "source"))
  names(expected) <- repos
  
  ## character //
  expect_equivalent(res <- getRepositoryType(repos = repos_0), expected)
  
  ## RappPackageRepositoryS3 //
  expect_equivalent(res <- getRepositoryType(repos = asRepository(repos_0)), expected)
  
  ## RappExpandedPackageRepositoryS3 //
  expect_equivalent(res <- getRepositoryType(repos=repos), expected)
  
  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  sapply(seq(along=repos), function(ii) {
    expect_equivalent(res <- getRepositoryType(repos=repos[[ii]]), expected[[ii]])
  })
  
  }
)

