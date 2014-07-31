context("package")

test_that("ensureRepository", {

  path  <- normalizePath(file.path(tempdir(), "repos"), winslash="/", mustWork=FALSE)
  path 	<- asRepository(path)
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  repos <- expandRepository(path=path)
  expected <- TRUE
  expect_equivalent(res <- ensureRepository(path=path), expected)
  
  on.exit(
  {
    if (grepl(basename(tempdir()), path)) {
      unlink(path, recursive=TRUE, force=TRUE)
    }
  }
    )
  
  }
)

