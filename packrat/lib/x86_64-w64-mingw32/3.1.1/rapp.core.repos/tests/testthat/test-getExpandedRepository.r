context("package")

test_that("getExpandedRepository", {

  path <- "c:"
  expected <- file.path(path, getPartialRepositoryScaffold())
  expect_equal(res <- getExpandedRepository(path=path), expected)

  path <- "."
  expected <- file.path(path, getPartialRepositoryScaffold())
  expect_equal(res <- getExpandedRepository(), expected)
  
  }
)

