repos  <- file.path(tempdir(), "repos")
repos  <- normalizePath(repos, winslash = "/", mustWork = FALSE)

##------------------------------------------------------------------------------
## File system //
##------------------------------------------------------------------------------

normalizeRepositoryPath(repos = repos)
  

##------------------------------------------------------------------------------
## URL (file system) //
##------------------------------------------------------------------------------

res <- normalizeRepositoryPath(repos = repos, type = "file")
normalizeRepositoryPath(repos = res, type = "file")

##------------------------------------------------------------------------------
## URL (http)
##------------------------------------------------------------------------------

normalizeRepositoryPath(repos = repos, type = "http")

##------------------------------------------------------------------------------
## URL (ftp) //
##------------------------------------------------------------------------------
  
res <- normalizeRepositoryPath(repos = repos, type = "ftp")
normalizeRepositoryPath(repos = res, type = "ftp")  

\dontrun{
  
##------------------------------------------------------------------------------
## Conditions //
##------------------------------------------------------------------------------
  
try(normalizeRepositoryPath(repos = repos, type = "nonexistingtype"))
  
}
