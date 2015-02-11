##------------------------------------------------------------------------------
context("ensureRepository")
##------------------------------------------------------------------------------

.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos_0  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))
.cleanTempDir(repos_0) 
repos_url <- file.path("file://", repos_0)
expected <- TRUE

test_that("ensureRepository/character", {
  
  expect_equivalent(res <- ensureRepository(repos = repos_0), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  expect_equivalent(res <- ensureRepository(repos = repos_url), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  on.exit(.cleanTempDir(x = repos_0))
  
})

test_that("ensureRepository/PackageRepositoryRoot.S3", {  
  
  expect_equivalent(res <- ensureRepository(
    repos = asRepositoryRoot(repos_0)), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  expect_equivalent(res <- ensureRepository(
    repos = asRepositoryRoot(repos_url)), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  on.exit(.cleanTempDir(x = repos_0))

})

test_that("ensureRepository/PackageRepository.S3", {  
  
  expect_equivalent(res <- ensureRepository(
    repos = asRepository(repos = repos_0)), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  expect_equivalent(res <- ensureRepository(
    repos = asRepository(repos = repos_url)), expected)
  expect_true(file.exists(repos_0))
  .cleanTempDir(x = repos_0)
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

