context("package")

test_that("getRepositoryRoot", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  repos  <- file.path(tempdir(), "repos")
  repos_0 <- repos
  
  ## Missing //
  expected <- addClassAttribute(obj = ".", 
    class_name = "RappPackageRepositoryS3")
  expect_equal(res <- getRepositoryRoot(), expected)
  
  ## Character //
  expected <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryS3")
  expect_equal(res <- getRepositoryRoot(repos = repos), expected)
  
  ## RappPackageRepositoryS3 //
  expect_equal(res <- getRepositoryRoot(repos = repos), expected)
  
  ## RappExpandedRappRepositoryS3 //
  expected <- asRepository(repos = repos_0)
  expect_equal(res <- getRepositoryRoot(repos = asExpandedRepository(repos)), expected)

  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  repos <- asExpandedRepository(repos = repos_0)
  expected <- asRepository(repos = repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_equal(res <- getRepositoryRoot(repos = repos[[ii]]), expected)
  })
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

