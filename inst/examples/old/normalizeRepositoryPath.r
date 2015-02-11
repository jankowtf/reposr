repos  <- file.path(tempdir(), "repos")
repos  <- normalizePath(repos, winslash = "/", mustWork = FALSE)

##------------------------------------------------------------------------------
## File system //
##------------------------------------------------------------------------------

normalizeRepositoryPath(repos = repos)
  

##------------------------------------------------------------------------------
## URL (file system) //
##------------------------------------------------------------------------------

res <- normalizeRepositoryPath(repos = repos, type = "url_fs")
normalizeRepositoryPath(repos = res, type = "url_fs")

##------------------------------------------------------------------------------
## URL (http)
##------------------------------------------------------------------------------

normalizeRepositoryPath(repos = repos, type = "url_http")

##------------------------------------------------------------------------------
## URL (ftp) //
##------------------------------------------------------------------------------
  
res <- normalizeRepositoryPath(repos = repos, type = "url_ftp")
normalizeRepositoryPath(repos = res, type = "url_ftp")  

\dontrun{
  
##------------------------------------------------------------------------------
## Conditions //
##------------------------------------------------------------------------------
  
try(normalizeRepositoryPath(repos = repos, type = "nonexistingtype"))
  
}
