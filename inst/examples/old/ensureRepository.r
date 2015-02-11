.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))
repos_url <- file.path("file://", repos)

##------------------------------------------------------------------------------
## Character //
##------------------------------------------------------------------------------

ensureRepository(repos = repos)
file.exists(repos)
.cleanTempDir(x = repos)
  
ensureRepository(repos = repos_url)
file.exists(repos)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## PackageRepositoryRoot.S3 //
##------------------------------------------------------------------------------
  
ensureRepository(repos = asRepositoryRoot(repos))
file.exists(repos)
.cleanTempDir(x = repos)
  
ensureRepository(repos = asRepositoryRoot(repos_url))
file.exists(repos)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## PackageRepository.S3 //
##------------------------------------------------------------------------------
  
ensureRepository(repos = asRepository(repos = repos))
file.exists(repos)
.cleanTempDir(x = repos)
  
ensureRepository(repos = asRepository(repos = repos_url))
file.exists(repos)
.cleanTempDir(x = repos)


  