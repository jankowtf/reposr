context("package")

test_that("package", {

  path  <- normalizePath(file.path(tempdir(), "repos"), winslash="/")
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  
  ## Get scaffold //
  repos <- getExpandedRepository(path=path)
  
  ## Ensure existence //
  ensureRepository(path=path)

  ## Get repository types //
  getRepositoryType(repos=repos)
  
  ## Get repository path by type //
  path_win <- getRepositoryPathByType(repos = repos)
  
  build(binary=TRUE, path=path_win)
  
  ## Refresh //
  refreshRepository(repos = repos)
  
  on.exit({
    if (grepl(basename(tempdir()), path)) {
        unlink(path, recursive=TRUE, force=TRUE)
    }
  })
  
  }
)

