context("package")

test_that("asExpandedRepository", {

  path  <- normalizePath(file.path(tempdir(), "repos"), winslash="/")
  expected <- "RappExpandedRepositoryS3"
  expect_is(res <- asExpandedRepository(path=path), expected)

  }
)

