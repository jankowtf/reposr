context("Class: PackageRepositoryRoot.S3")

test_that("PackageRepositoryRoot.S3/bare", {

  expect_is(res <- PackageRepositoryRoot.S3(), "PackageRepositoryRoot.S3")
  fields <- c(
    "path", 
    "type"
  )
  expect_true(all(fields %in% names(res)))
  
})

test_that("PackageRepositoryRoot.S3/fields", {
  
  expect_is(res <- PackageRepositoryRoot.S3(
    path = "a/b/c", type = "fs"), "PackageRepositoryRoot.S3")
  expect_identical(res$path, "a/b/c")
  expect_identical(res$type, "fs")
  
})
