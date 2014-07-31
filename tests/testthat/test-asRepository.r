context("package")

test_that("asRepository", {

  path  <- normalizePath(file.path(tempdir(), "repos"), winslash="/")
  expected <- "RappRepositoryS3"
  expect_is(res <- asRepository(path=path), expected)

  }
)

