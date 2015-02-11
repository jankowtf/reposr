# rversion <- "3.1"
rversion <- paste(R.version$major, 
               unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
expected <- c(
  file.path(c("bin/windows/contrib", "bin/macosx/contrib"), rversion), 
  "src/contrib"
)
names(expected) <- c("win.binary", "mac.binary", "source")

##------------------------------------------------------------------------------
context("getRelativeRepositorySubPaths/rversion")
##------------------------------------------------------------------------------

test_that("getRelativeRepositorySubPaths/rversion", {

  expect_equal(res <- getRelativeRepositorySubPaths(rversion = rversion), 
    sort(expected))
  
})

##------------------------------------------------------------------------------
context("getRelativeRepositorySubPaths/no rversion")
##------------------------------------------------------------------------------

test_that("getRelativeRepositorySubPaths/no rversion", {
  
  expect_equal(res <- getRelativeRepositorySubPaths(), sort(expected))
  
})
