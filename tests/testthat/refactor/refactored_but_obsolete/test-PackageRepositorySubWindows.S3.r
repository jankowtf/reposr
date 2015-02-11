##------------------------------------------------------------------------------
context("Class: PackageRepositorySubWindows.S3")
##------------------------------------------------------------------------------

test_that("PackageRepositorySubWindows.S3/bare", {

  expect_is(res <- PackageRepositorySubWindows.S3(), 
            "PackageRepositorySubWindows.S3")
  fields <- c(
    "path"
  )
  expect_true(all(fields %in% names(res)))
  
})


test_that("PackageRepositorySubWindows.S3/fields", {
  
  expect_is(res <- PackageRepositorySubWindows.S3(
    path = "a/b/c"), "PackageRepositorySubWindows.S3")
  expect_identical(res$path, "a/b/c")
  
})
