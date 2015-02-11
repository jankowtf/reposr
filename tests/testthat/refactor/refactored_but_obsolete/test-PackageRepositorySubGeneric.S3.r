##------------------------------------------------------------------------------
context("Class: PackageRepositorySubGeneric.S3")
##------------------------------------------------------------------------------

test_that("PackageRepositorySubGeneric.S3/bare", {

  expect_is(res <- PackageRepositorySubGeneric.S3(), 
            "PackageRepositorySubGeneric.S3")
  fields <- c(
    "path"
  )
  expect_true(all(fields %in% names(res)))
  
})


test_that("PackageRepositorySubGeneric.S3/fields", {
  
  expect_is(res <- PackageRepositorySubGeneric.S3(
    path = "a/b/c"), "PackageRepositorySubGeneric.S3")
  expect_identical(res$path, "a/b/c")
  
})
