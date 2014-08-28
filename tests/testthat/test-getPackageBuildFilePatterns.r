context("package")

test_that("getPackageBuildFilePatterns", {

#   repos_home <- file.path(tempdir(), "repos")
#   ensureRepository(repos = repos_home)
#   path <- getRepositoryPathByType(repos_home)
  
  if (basename(getwd()) == "testthat") {
    input <- "PACKAGES"
  } else {
    input <- "tests/testthat/PACKAGES"
  }
  
  expected <- c("package.a_0.1.0.1\\.\\w.*$", "package.b_1.0\\.\\w.*$", 
                "package.c_0.1.2.3\\.\\w.*$")
  expect_equal(res <- getPackageBuildFilePatterns(input = input), expected)
  
  expect_equal(res <- getPackageBuildFilePatterns(
    input = parseRepositoryIndexFile(path = input)), expected)

  if (basename(getwd()) == "testthat") {
    input <- "repos"
  } else {
    input <- "tests/testthat/repos"
  }
  
  ## RappPackageRepositoryS3 //
  input_this <- asRepository(repos = input) 
  expected <- list(
    NA, 
    c("dummy.package_1.2\\.\\w.*$", "dummy.package2_1.2\\.\\w.*$"),
    c("dummy.package_1.2\\.\\w.*$", "dummy.package2_1.2\\.\\w.*$")
  )
  names(expected) <- asExpandedRepository(input_this)
  expect_equal(res <- getPackageBuildFilePatterns(input = input_this), 
               expected)
  
  ## RappExpandedPackageRepositoryS3 //
  input_this <- asExpandedRepository(repos = input) 
  expect_equal(res <- getPackageBuildFilePatterns(input = input_this), 
               expected)
  
  ## RappPackageRepositoryMacBinaryS3
  ## RappPackageRepositoryWinBinaryS3
  ## RappPackageRepositorySourceS3
  sapply(seq(along=input_this), function(ii) {
    expect_equivalent(res <- getPackageBuildFilePatterns(
      input = input_this[[ii]]), expected[ii])
  })
  
  }
)
