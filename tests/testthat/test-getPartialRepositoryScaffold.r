context("getPartialRepositoryScaffold-A")

test_that("getPartialRepositoryScaffold", {

  rversion <- "3.1"
  expected <- c(
    file.path(c("bin/windows/contrib", "bin/macosx/contrib"), rversion), 
    "src/contrib"
  )
  names(expected) <- c("win.binary", "mac.binary", "source")
  expect_equal(res <- getPartialRepositoryScaffold(rversion = rversion), 
    sort(expected))
  expect_equal(res <- getPartialRepositoryScaffold(), sort(expected))
  
  }
)
