context("removeOldRepositoryPackages-A")

test_that("removeOldRepositoryPackages", {

  repos_to <- file.path(tempdir(), "repos")
  
  if (basename(getwd()) == "testthat") {
    repos_from <- "repos"
  } else {
    repos_from <- "tests/testthat/repos"
  }
  if (!file.exists(repos_from)) {
    stop("Test repository does not exist")
  }
  
  ## Just once //
#   ensureRepository(repos = repos_from)
#   repos_expanded <- asRepository(repos_from)
#   devtools::build(path = normalizePath(repos_expanded["source"], winslash = "/"))
#   devtools::build(path = normalizePath(repos_expanded["win.binary"], winslash = "/"))
#   refreshRepositoryIndex(repos_to)

  
  expected <- rep(TRUE, 3)
  nms <- sort(c("mac.binary", "source", "win.binary"))
  names(expected) <- nms

  ## Character //
  file.copy(from = repos_from, to = tempdir(), recursive = TRUE)
  expect_equal(res <- removeOldRepositoryPackages(repos = repos_to), expected)

  ## PackageRepositoryRoot.S3 //
  file.copy(from = repos_from, to = tempdir(), recursive = TRUE)
  expect_equal(res <- removeOldRepositoryPackages(
    repos = asRepositoryRoot(repos = repos_to)), expected)

  ## PackageRepository.S3 //
  file.copy(from = repos_from, to = tempdir(), recursive = TRUE)
  expect_equal(res <- removeOldRepositoryPackages(
    repos = asRepository(repos = repos_to)), expected)
  expect_equal(res <- removeOldRepositoryPackages(
    repos = asRepository(repos = repos_to)), expected)

  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubSource.S3
  repos <- asRepository(repos = repos_to)
  sapply(seq(along=repos), function(ii) {
    file.copy(from = repos_from, to = tempdir(), recursive = TRUE)
    expect_equivalent(res <- removeOldRepositoryPackages(
      repos = repos[[ii]]), expected[ii])
  })

  on.exit({
    if (grepl(basename(tempdir()), repos_to)) {
      unlink(repos_to, recursive = TRUE, force = TRUE)
    }
  })

  }
)
