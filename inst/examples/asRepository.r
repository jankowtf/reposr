.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}

repos  <- normalizePath(file.path(tempdir(), "repos"), 
  winslash = "/", mustWork = FALSE)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## Character //
##------------------------------------------------------------------------------

asRepository(repos = repos)
file.exists(repos)

asRepository(repos = repos, ensure = TRUE)
file.exists(repos)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## PackageRepositoryRoot.S3 //
##------------------------------------------------------------------------------

asRepository(repos = asRepositoryRoot(repos))
file.exists(repos)

asRepository(repos = asRepositoryRoot(repos), ensure = TRUE)
file.exists(repos)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## PackageRepository.S3 //
##------------------------------------------------------------------------------

asRepository(repos = asRepository(repos))
file.exists(repos)

asRepository(repos = asRepository(repos), ensure = TRUE)
file.exists(repos)
.cleanTempDir(x = repos)

##------------------------------------------------------------------------------
## Sublevels //
##------------------------------------------------------------------------------

## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3
## PackageRepositorySubWindows.S3

repos <- asRepository(repos)
lapply(seq(along = repos$sublevel), function(ii) {
  asRepository(repos = repos$sublevel[[ii]])
})
lapply(seq(along = repos$sublevel), function(ii) {
  asRepository(repos = repos$sublevel[[ii]], ensure = TRUE)
})
.cleanTempDir(x = repos$root$path)
