context("package")

test_that("getPartialRepositoryScaffold", {

  rversion <- "3.1"
  expected <- file.path(
    c("bin/windows/contrib", "bin/macosx/contrib", "src"), rversion)
  expect_equal(res <- getPartialRepositoryScaffold(rversion=rversion), sort(expected))
  expect_equal(res <- getPartialRepositoryScaffold(), sort(expected))
  
  }
)
selectMethod("getPartialRepositoryScaffold", "character")
