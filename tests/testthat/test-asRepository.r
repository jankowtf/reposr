.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos_0  <- normalizePath(file.path(tempdir(), "repos"), 
  winslash = "/", mustWork = FALSE)
.cleanTempDir(x = repos_0)

##------------------------------------------------------------------------------
context("asRepository/character")
##------------------------------------------------------------------------------

test_that("asRepository/character", {

  expected <- "PackageRepository"
  expect_is(res <- asRepository(repos = repos_0), expected)
  expect_false(file.exists(repos_0))
  expect_is(res <- asRepository(repos = repos_0, ensure = TRUE), expected)
  expect_true(file.exists(repos_0))
  on.exit(.cleanTempDir(x = repos_0))
  
})



##------------------------------------------------------------------------------
context("asRepository/PackageRepository")
##------------------------------------------------------------------------------

test_that("asRepository/PackageRepository", {
  
  expected <- "PackageRepository"
  expect_is(res <- asRepository(repos = asRepository(repos_0)), expected)
  expect_false(file.exists(repos_0))
  
  expect_is(res <- asRepository(repos = asRepository(repos_0), ensure = TRUE), 
    expected)
  expect_true(file.exists(repos_0))
  on.exit(.cleanTempDir(x = repos_0))
  
})

##------------------------------------------------------------------------------
context("asRepository/sublevel")
##------------------------------------------------------------------------------

test_that("asRepository/sublevel", {
  
  expected <- "PackageRepository"
  repos <- asRepository(repos_0)
  sapply(seq(along=repos$sublevel), function(ii) {
    expect_is(res <- asRepository(repos = repos$sublevel[[ii]]), expected)
  })
  on.exit(.cleanTempDir(x = repos_0))
  
})
