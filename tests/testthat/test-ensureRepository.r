context("package")

test_that("ensureRepository", {

  .cleanTempDir <- function(x) {
    if (grepl(basename(tempdir()), x)) {
      unlink(x, recursive = TRUE, force = TRUE)
    }
  }
  
  expected <- TRUE
  repos  <- normalizePath(file.path(tempdir(), "repos"), winslash="/", 
                         mustWork=FALSE)
  repos_0 <- repos
  expect_equivalent(res <- ensureRepository(repos = repos), expected)
  .cleanTempDir(x = repos_0)
  
  expect_equivalent(res <- ensureRepository(repos = asRepository(repos)), expected)
  .cleanTempDir(x = repos_0)

  expect_equivalent(res <- ensureRepository(repos = asExpandedRepository(repos = repos)), expected)
  
  on.exit(.cleanTempDir(x = repos_0))
  
  on.exit(.cleanTempDir(x = repos_0))
  
  }
)

