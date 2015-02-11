context("addClassAttribute")
test_that("addClassAttribute", {

  obj <- "hello world!"
  expected <- c(
    "PackageRepository.S3",
    "PackageRepositoryRoot.S3",
    "PackageRepositorySubGeneric.S3",
    "PackageRepositorySubMac.S3",
    "PackageRepositorySubSource.S3",
    "PackageRepositorySubMac.S3",
    "RappPackageRepositoryListS3",
    "RappParsedPackageRepositoryIndexS3"
  )
  lapply(expected, function(ii) {
    expect_is(res <- addClassAttribute(obj = obj, class_name = ii), ii)
  })
  
  expect_error(addClassAttribute(obj = obj, class_name = "NonExistingClass"))
  
  }
)

