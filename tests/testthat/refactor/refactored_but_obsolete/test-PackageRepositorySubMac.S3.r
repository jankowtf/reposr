##------------------------------------------------------------------------------
context("Class: PackageRepositorySubMac.S3")
##------------------------------------------------------------------------------

test_that("PackageRepositorySubMac.S3/bare", {

  expect_is(res <- PackageRepositorySubMac.S3(), 
            "PackageRepositorySubMac.S3")
  fields <- c(
    "path"
  )
  expect_true(all(fields %in% names(res)))
  
})


test_that("PackageRepositorySubMac.S3/fields", {
  
  expect_is(res <- PackageRepositorySubMac.S3(
    path = "a/b/c"), "PackageRepositorySubMac.S3")
  expect_identical(res$path, "a/b/c")
  
})
