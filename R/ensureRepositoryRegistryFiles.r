#' Ensure Repository Registry Files
#'
#' @description 
#' Ensures existence of a repository's registry files (\code{PACKAGES} and
#' \code{PACKAGES.gz}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/ensureRepositoryRegistryFiles.R
#' @seealso \code{\link[rapp2]{ensureRepositoryRegistryFiles-character-method}}
#' @export ensureRepositoryRegistryFiles
setGeneric(name="ensureRepositoryRegistryFiles", 
  signature = c(
    "repos"
  ),
  def = function(
      repos
  ) {
  standardGeneric("ensureRepositoryRegistryFiles")
})

#' @param repos \code{\link{RappExpandedRepositoryS3}}. 
#' @return TODO 
#' @describeIn ensureRepositoryRegistryFiles
#' @export
setMethod(f = "ensureRepositoryRegistryFiles", 
  signature = signature(
      repos = "RappExpandedRepositoryS3"
  ), 
  definition = function(
      repos
  ) {
     
  sapply(repos, function(ii) {
    type <- getRepositoryType(repos=ii)    
    fpath <- file.path(ii, c("PACKAGES", "PACKAGES.gz"))
    if (!all(file.exists(fpath))) {
      wd_0   <- getwd()
      tryCatch(
        {
          setwd(ii)
          
  #         tools::write_PACKAGES(".", type=.Platform$pkgType)
          tools::write_PACKAGES(".", type=type)                
        },
        finally=setwd(wd_0)
      )
    }
  })
  
  } 
)

