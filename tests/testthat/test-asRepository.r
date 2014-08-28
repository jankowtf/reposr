context("package")

test_that("asRepository", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  repos_0  <- file.path(tempdir(), "repos")
  
  ## character //
  expected <- "RappPackageRepositoryS3"
  expect_is(res <- asRepository(repos = repos_0), expected)
  
  ## RappPackageRepositoryS3 //
  expect_is(res <- asRepository(repos = asRepository(repos_0)), expected)
  
  ## RappExpandedRappRepositoryS3 //
  expect_is(res <- asRepository(repos = asExpandedRepository(repos_0)), expected)

  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  repos <- asExpandedRepository(repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_is(res <- asRepository(repos = repos[[ii]]), expected)
  })
  
  ## Ensure //
  expect_true(file.exists(asRepository(repos = repos_0, ensure = TRUE)))
  .cleanTempDir(x = repos_0)
  
  expect_true(file.exists(
    asRepository(repos = asRepository(repos_0), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  
  expect_true(file.exists(
    asRepository(repos = asExpandedRepository(repos_0), ensure = TRUE)
  ))
  .cleanTempDir(x = repos_0)
  
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

