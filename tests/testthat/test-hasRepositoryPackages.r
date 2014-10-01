context("hasRepositoryPackages-A")

test_that("hasRepositoryPackages", {

  if (basename(getwd()) == "testthat") {
    repos <- "repos"
  } else {
    repos <- "tests/testthat/repos"
  }
  if (!file.exists(repos)) {
    stop("Test repository does not exist")
  }
  repos_0 <- repos
  
  nms <- asExpandedRepository(repos = repos_0)
  expected <- c(FALSE, TRUE, TRUE)
  names(expected) <- nms
  
  ## character //
  expect_equal(res <- hasRepositoryPackages(repos = repos), expected)

  ## RappPackageRepositoryS3 //
  expect_equal(res <- hasRepositoryPackages(
    repos = asRepository(repos = repos_0)), expected)
  
  ## RappExpandedPackageRepositoryS3 //
  expect_equal(res <- hasRepositoryPackages(
    repos = asExpandedRepository(repos = repos_0)), expected)
  
  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  repos <- asExpandedRepository(repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_equal(res <- hasRepositoryPackages(repos=repos[[ii]]), expected[ii])
  })
  
  repos <- file.path(tempdir(), "repos")
  ensureRepository(repos)
  expected <- rep(FALSE, 3)
  names(expected) <- asExpandedRepository(repos = repos)
  expect_equal(res <- hasRepositoryPackages(repos = repos), expected)
  
  on.exit({
    if (grepl(basename(tempdir()), repos_0)) {
      unlink(repos_0, recursive = TRUE, force = TRUE)
    }
  })
  
  }
)

