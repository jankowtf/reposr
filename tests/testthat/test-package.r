context("package")

test_that("package", {

  skip("manual only")
  repos  <- normalizePath(file.path(tempdir(), "repos"), winslash="/",
                         mustWork=FALSE)
  dir.create(repos, recursive=TRUE, showWarnings=FALSE)
  
  ## As repository //
  repos <- asRepository(repos = repos)
  
  ## Get scaffold //
  repos <- asExpandedRepository(repos = repos)
  
  ## Ensure existence //
  ensureRepository(repos = repos)

  ## Get repository types //
  getRepositoryType(repos = repos)  
  getRepositoryType(repos = repos[1])
  getRepositoryType(repos = repos[2])
  getRepositoryType(repos = repos[3])
   
  ## Get repository path by type //
  repos_win <- getRepositoryPathByType(repos = repos)
  
  ## Build directly into repository //
  if (FALSE) {
    require("devtools")
    build(binary=TRUE, path = repos_win)
  }
  
  ## Refresh //
  refreshRepositoryIndex(repos = repos)
  
  ## Set local repository //
  setLocalPackratRepository(repos = as.character(repos))
  
  ## Reset local repository //
  resetLocalPackratRepository()
  
  on.exit({
    if (any(grepl(basename(tempdir()), repos))) {
        unlink(repos, recursive = TRUE, force = TRUE)
    }
  })
  
  }
)

