repos_0  <- gsub("\\\\", "/", file.path(tempdir(), "repos"))

##------------------------------------------------------------------------------
## Missing //
##------------------------------------------------------------------------------

getRepositoryRoot()
  
##------------------------------------------------------------------------------
## Character //
##------------------------------------------------------------------------------
  
getRepositoryRoot(repos = repos_0)
  
##------------------------------------------------------------------------------
## PackageRepositoryRoot.S3 //
##------------------------------------------------------------------------------

repos <- PackageRepositoryRoot.S3(path = repos_0)
getRepositoryRoot(repos = repos)
  
##------------------------------------------------------------------------------
## PackageRepository.S3 //
##------------------------------------------------------------------------------

getRepositoryRoot(repos = asRepository(repos_0))
  
##------------------------------------------------------------------------------
## Sublevels //
##------------------------------------------------------------------------------

## PackageRepositorySubMac.S3
## PackageRepositorySubSource.S3
## PackageRepositorySubWindows.S3
  
repos <- asRepository(repos = repos_0)
lapply(seq(along = repos$sublevel), function(ii) {
  getRepositoryRoot(repos = repos$sublevel[[ii]])
})
  
