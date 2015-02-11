context("getRepositoryPathByType-A")

test_that("getRepositoryPathByType", {

  repos <- file.path(tempdir(), "repos")
  dir.create(repos, recursive = TRUE, showWarnings = FALSE)
  repos <- normalizePath(repos, winslash = "/")
  repos_0 <- repos
  
  rversion <- paste(R.version$major, 
      unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
  
  ## Character //
  expected <- file.path(repos, "bin/windows/contrib", rversion)
  expected <- addClassAttribute(obj = expected, 
    class_name = "PackageRepositorySubMac.S3")
  expected_win <- list(win.binary = expected)
  expect_equal(res <- getRepositoryPathByType(repos = repos), expected_win)
  
  expected <- file.path(repos, "src/contrib")
  expected <- addClassAttribute(obj = expected, 
    class_name = "PackageRepositorySubSource.S3")
  expected_source <- list(source = expected)
  
  expect_equal(res <- getRepositoryPathByType(repos = repos, type = "source"), 
     expected_source)
  
  expected <- file.path(repos, "bin/macosx/contrib", rversion)
  expected <- addClassAttribute(obj = expected, 
    class_name = "PackageRepositorySubMac.S3")
  expected_mac <- list(mac.binary = expected)
  expect_equal(res <- getRepositoryPathByType(repos = repos, 
    type = "mac.binary"), expected_mac)
  
  ## PackageRepositoryRoot.S3 //
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepositoryRoot(repos_0)), expected_win)
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepositoryRoot(repos_0), type = "source"), expected_source)
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepositoryRoot(repos_0), type = "mac.binary"), expected_mac)

  ## PackageRepository.S3 //
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepository(repos_0)), expected_win)
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepository(repos_0), type = "source"), expected_source)
  expect_equal(res <- getRepositoryPathByType(
    repos = asRepository(repos_0), type = "mac.binary"), expected_mac)

  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubMac.S3
  ## PackageRepositorySubSource.S3
  repos <- asRepository(repos_0)
  expected <- c(expected_mac, expected_source, expected_win)  
  sapply(seq(along=repos), function(ii) {
    expect_equal(res <- getRepositoryPathByType(repos = repos[[ii]]), expected[[ii]])
  })
  
}
)

