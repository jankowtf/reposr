context("package")

test_that("setLocalPackratRepository", {

  opts_old <- packrat::get_opts("local.repos")
  
  ## character //
  repos <- "c:/"
  expected <- TRUE
  expect_equal(res <- setLocalPackratRepository(repos = repos), expected)
  expect_equal(packrat::get_opts("local.repos"), repos)
  
  expect_equal(res <- setLocalPackratRepository(repos = repos), expected)
  expect_equal(packrat::get_opts("local.repos"), repos)
  
  repos_0 <- repos
  repos <- "c:/temp"
  expect_equal(res <- setLocalPackratRepository(repos = repos), expected)
  expect_equal(packrat::get_opts("local.repos"), c(repos_0, repos))
  
  expect_equal(res <- setLocalPackratRepository(
    repos = repos, append_existing = FALSE), expected)
  expect_equal(packrat::get_opts("local.repos"), repos)
  
  ## RappPackageRepositoryS3 //
  expect_equal(res <- setLocalPackratRepository(
    repos = asRepository(repos), append_existing = FALSE), expected)
  expect_equal(packrat::get_opts("local.repos"), repos)
    
  packrat::set_opts(local.repos = opts_old)
  
  }
)

