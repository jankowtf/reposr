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
  
  nms <- asRepository(repos = repos_0)
  expected <- c(FALSE, TRUE, TRUE)
  names(expected) <- nms
  
  ## character //
  expect_equal(res <- hasRepositoryPackages(repos = repos), expected)

  ## PackageRepositoryRoot.S3 //
  expect_equal(res <- hasRepositoryPackages(
    repos = asRepositoryRoot(repos = repos_0)), expected)
  
  ## PackageRepository.S3 //
  expect_equal(res <- hasRepositoryPackages(
    repos = asRepository(repos = repos_0)), expected)
  
  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubSource.S3
  repos <- asRepository(repos_0)
  sapply(seq(along=repos), function(ii) {
    expect_equal(res <- hasRepositoryPackages(repos=repos[[ii]]), expected[ii])
  })
  
  repos <- file.path(tempdir(), "repos")
  ensureRepository(repos)
  expected <- rep(FALSE, 3)
  names(expected) <- asRepository(repos = repos)
  expect_equal(res <- hasRepositoryPackages(repos = repos), expected)
  
  on.exit({
    if (grepl(basename(tempdir()), repos_0)) {
      unlink(repos_0, recursive = TRUE, force = TRUE)
    }
  })
  
  }
)

