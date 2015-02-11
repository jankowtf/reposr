##------------------------------------------------------------------------------
context("Class: PackageRepositorySubSource.S3")
##------------------------------------------------------------------------------

test_that("PackageRepositorySubSource.S3/bare", {

  expect_is(res <- PackageRepositorySubSource.S3(), 
            "PackageRepositorySubSource.S3")
  fields <- c(
    "path"
  )
  expect_true(all(fields %in% names(res)))
  
})


test_that("PackageRepositorySubSource.S3/fields", {
  
  expect_is(res <- PackageRepositorySubSource.S3(
    path = "a/b/c"), "PackageRepositorySubSource.S3")
  expect_identical(res$path, "a/b/c")
  
})
