context("package")

test_that("getExpandedRepositoryPaths", {

  repos <- file.path(tempdir(), "repos")
  partial <- getPartialRepositoryScaffold()
  expected <- as.list(file.path(repos, partial))
  nms <- names(partial)
  names(expected) <- nms
  expect_equal(res <- getExpandedRepositoryPaths(repos = repos), 
               expected[sort(names(expected))])

  repos <- "."
  partial <- getPartialRepositoryScaffold()
  expected <- as.list(file.path(repos, partial))
  nms <- names(partial)
  names(expected) <- nms
  expect_equal(res <- getExpandedRepositoryPaths(),
               expected[sort(names(expected))])
  
  }
)

