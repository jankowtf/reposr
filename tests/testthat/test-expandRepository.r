context("package")

test_that("expandRepository", {

  path <- "c:"
  expected <- file.path(path, getPartialRepositoryScaffold())
  class(expected) <- c("RappExpandedRepositoryS3", class(expected))
  expect_equal(res <- expandRepository(path=path), expected)

  path <- "."
  expected <- file.path(path, getPartialRepositoryScaffold())
  class(expected) <- c("RappExpandedRepositoryS3", class(expected))
  expect_equal(res <- expandRepository(), expected)
  
  }
)

