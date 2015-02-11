context("setLocalPackratRepository-A")

test_that("setLocalPackratRepository", {

  if ("packrat" %in% list.files(.libPaths()[1])) {
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
    
    ## PackageRepositoryRoot.S3 //
    expect_equal(res <- setLocalPackratRepository(
      repos = asRepositoryRoot(repos), append_existing = FALSE), expected)
    expect_equal(packrat::get_opts("local.repos"), repos)
      
    packrat::set_opts(local.repos = opts_old)
  }
  
  }
)

