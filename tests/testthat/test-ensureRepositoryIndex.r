context("package")

test_that("ensureRepositoryIndex", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  repos_0 <- file.path(tempdir(), "repos")
  repos <- asExpandedRepository(repos = repos_0)
  sapply(repos, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  ## character //
  nms <- repos
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndex(
    repos = repos_0), expected)
  
  ## RappPackageRepositoryS3 //
  nms <- repos
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndex(
    repos = asRepository(repos = repos)), expected)
  
  ## RappExpandedPackageRepositoryS3 //
  nms <- repos
  expected <- rep(TRUE, length(nms))
  names(expected) <- nms
  expect_equivalent(res <- ensureRepositoryIndex(repos = repos), expected)
  
  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  sapply(seq(along=repos), function(ii) {
    expect_equivalent(res <- ensureRepositoryIndex(repos = repos[[ii]]), expected[[ii]])
  })
  
  ## Condition handling //
  .cleanTempDir(x = repos_0)
  expect_error(res <- ensureRepositoryIndex(repos = repos))
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

