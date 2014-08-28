context("package")

test_that("parseRepositoryIndexFile", {

  repos <- file.path(tempdir(), "repos")
  ensureRepository(repos = repos)
#   path <- getRepositoryPathByType(repos)
  
  if (basename(getwd()) == "testthat") {
    path <- "PACKAGES"
  } else {
    path <- "tests/testthat/PACKAGES"
  }
  
  expected <- "RappParsedPackageRepositoryIndexS3"
  expect_is(res <- parseRepositoryIndexFile(path = path), expected)
  expect_equal(res$Package, paste0("package.", c(letters[1:3])))
  
  if (basename(getwd()) == "testthat") {
    path <- "repos/src/contrib/PACKAGES"
  } else {
    path <- "tests/testthat/repos/src/contrib/PACKAGES"
  }

  expect_is(res <- parseRepositoryIndexFile(path = path), expected)
  expect_equal(res$Package, paste0("dummy.package", c("", "2")))

  }
)
