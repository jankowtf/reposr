context("resetLocalPackratRepository-A")

test_that("resetLocalPackratRepository", {

  if ("packrat" %in% list.files(.libPaths()[1])) {
    repos <- "c:/"
    setLocalPackratRepository(repos = repos)
    expect_equal(packrat::get_opts("local.repos"), repos)
    
    expected <- TRUE
    expect_equal(resetLocalPackratRepository(), expected)
    expect_equal(packrat::get_opts("local.repos"), character())
  }
  
  }
)

