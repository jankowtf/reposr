context("package")

test_that("package", {

  path  <- normalizePath(file.path(tempdir(), "repos"), winslash="/")
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  
  ## As repository //
  path <- asRepository(path = path)
  
  ## Get scaffold //
  repos <- expandRepository(path = path)
  
  ## Ensure existence //
  ensureRepository(path = path)

  ## Get repository types //
  getRepositoryType(repos = repos)  
  getRepositoryType(repos = repos[1])
  getRepositoryType(repos = repos[2])
  getRepositoryType(repos = repos[3])
   
  ## Get repository path by type //
  path_win <- getRepositoryPathByType(repos = repos)
  
  ## Build directly into repository //
  build(binary=TRUE, path = path_win)
  
  ## Refresh //
  refreshRepository(repos = repos)
  
  on.exit({
    if (grepl(basename(tempdir()), path)) {
        unlink(path, recursive=TRUE, force=TRUE)
    }
  })
  
  }
)

