##------------------------------------------------------------------------------
context("getExpandedRepositoryPaths/explicit")
##------------------------------------------------------------------------------

test_that("getExpandedRepositoryPaths/explicit", {

  repos <- file.path(tempdir(), "repos")
  dir.create(repos, showWarnings = FALSE)
  repos <- normalizePath(repos, winslash = "/")
  partial <- getRelativeRepositorySubPaths()
  expected <- as.list(gsub("\\\\", "/", file.path(repos, partial)))
  nms <- names(partial)
  names(expected) <- nms
  expect_equal(res <- getExpandedRepositoryPaths(repos = repos), 
               expected[sort(names(expected))])

})

##------------------------------------------------------------------------------
context("getExpandedRepositoryPaths/expanded")
##------------------------------------------------------------------------------

test_that("getExpandedRepositoryPaths/expanded", {
  
  repos <- "."
  partial <- getRelativeRepositorySubPaths()
  expected <- as.list(file.path(normalizePath(repos, winslash = "/"), partial))
  nms <- names(partial)
  names(expected) <- nms
  expect_equal(res <- getExpandedRepositoryPaths(),
               expected[sort(names(expected))])
  
})

