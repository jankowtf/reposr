context("package")

test_that("asExpandedRepository", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  repos_0  <- normalizePath(file.path(tempdir(), "repos"), winslash = "/", 
                          mustWork = FALSE)
  
  ## character //
  expected <- "RappExpandedPackageRepositoryS3"
  expect_is(res <- asExpandedRepository(repos = repos_0), expected)
  expect_false(file.exists(repos_0))
  expect_is(res <- asExpandedRepository(repos = repos_0, ensure = TRUE), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  ## RappPackageRepositoryS3 //
  expected <- "RappExpandedPackageRepositoryS3"
  expect_is(res <- asExpandedRepository(repos = asRepository(repos_0)), expected)
  expect_false(file.exists(repos_0))
  expect_is(res <- asExpandedRepository(repos = asRepository(repos_0), ensure = TRUE), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  ## RappPackageRepositoryS3 //
  expected <- "RappExpandedPackageRepositoryS3"
  expect_is(res <- asExpandedRepository(repos = asExpandedRepository(repos_0)), expected)
  expect_false(file.exists(repos_0))
  expect_is(res <- asExpandedRepository(repos = asExpandedRepository(repos_0), ensure = TRUE), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  repos <- asExpandedRepository(repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_is(res <- asExpandedRepository(repos=repos[[ii]]), expected)
  })
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

