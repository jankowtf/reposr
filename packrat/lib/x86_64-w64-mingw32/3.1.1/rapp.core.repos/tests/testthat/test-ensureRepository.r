context("package")

test_that("ensureRepository", {

  path  <- file.path(tempdir(), "repos")
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  repos <- getExpandedRepository(path=path)
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

