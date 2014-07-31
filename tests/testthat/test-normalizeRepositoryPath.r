context("package")

test_that("normalizeRepositoryPath", {

  path  <- file.path(tempdir(), "repos")
  expected <- path
  expect_equivalent(res <- normalizeRepositoryPath(path = path), expected)
  expected <- paste0("file:///", path)
  expect_equivalent(res <- normalizeRepositoryPath(path = path, 
      type = "url"), expected)
  expect_equivalent(res <- normalizeRepositoryPath(path = res, 
      type = "url"), expected)
  expected <- path
  expect_equivalent(res <- normalizeRepositoryPath(path = res), expected)
    
#  path  <- asRepository(path = path)
#  expected <- path
#  expect_equivalent(res <- normalizeRepositoryPath(path = path), expected)
#  expected <- paste0("file:///", path)
#  expect_equivalent(res <- normalizeRepositoryPath(path = path, 
#      type = "url"), expected)
#  expect_equivalent(res <- normalizeRepositoryPath(path = res, 
#      type = "url"), expected)
#  expected <- path
#  expect_equivalent(res <- normalizeRepositoryPath(path = res), expected)
  
  }
)

