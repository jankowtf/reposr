context("package")

test_that("addClassAttribute", {

  obj <- "hello world!"
  expected <- c(
    "RappExpandedPackageRepositoryS3",
    "RappPackageRepositoryS3",
    "RappPackageRepositoryGenericS3",
    "RappPackageRepositoryMacBinaryS3",
    "RappPackageRepositorySourceS3",
    "RappPackageRepositoryWinBinaryS3",
    "RappPackageRepositoryListS3",
    "RappParsedPackageRepositoryIndexS3"
  )
  lapply(expected, function(ii) {
    expect_is(res <- addClassAttribute(obj = obj, class_name = ii), ii)
  })
  
  expect_error(addClassAttribute(obj = obj, class_name = "NonExistingClass"))
  
  }
)

