context("ensureRepository-A")
test_that("ensureRepository", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  expected <- TRUE
  repos  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))
  repos_0 <- repos
  repos_url <- file.path("file://", repos_0)
  
  ## Character //
  expect_equivalent(res <- ensureRepository(repos = repos), expected)
  .cleanTempDir(x = repos_0)
  expect_equivalent(res <- ensureRepository(repos = repos_url), expected)
  .cleanTempDir(x = repos_0)
  
  ## RappPackageRepositoryS3 //
  expect_equivalent(res <- ensureRepository(repos = asRepository(repos)), expected)
  .cleanTempDir(x = repos_0)
  expect_equivalent(res <- ensureRepository(repos = asRepository(repos_url)), expected)
  .cleanTempDir(x = repos_0)

  ## RappPackageExpandedRepositoryS3 //
  expect_equivalent(res <- ensureRepository(repos = asExpandedRepository(repos = repos)), expected)
  .cleanTempDir(x = repos_0)
  expect_equivalent(res <- ensureRepository(
    repos = asExpandedRepository(repos = repos_url)), expected)
  .cleanTempDir(x = repos_0)
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

