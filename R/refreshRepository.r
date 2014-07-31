#' Refresh Repository
#'
#' @description 
#' Refreshes repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param remove_old \code{\link{logical}}.
#'    Remove old content (\code{TRUE}) or not (\code{FALSE} (default)).
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/refreshRepository.R
#' @export refreshRepository
setGeneric(name="refreshRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    remove_old = FALSE
  ) {
  standardGeneric("refreshRepository")
})

#' @param repos \code{\link{RappExpandedRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepository
#' @export
setMethod(f = "refreshRepository", 
  signature = signature(
    repos = "RappExpandedRepositoryS3"
  ), 
  definition = function(
    repos,
    remove_old
  ) {
     
  sapply(repos, function(ii) {
    type <- getRepositoryType(repos=ii)    
    fpath <- file.path(ii, c("PACKAGES", "PACKAGES.gz"))
    wd_0   <- getwd()
    tryCatch(
      {
        setwd(ii)
        
#         tools::write_PACKAGES(".", type=.Platform$pkgType)
        tools::write_PACKAGES(".", type=type)                
      },
      finally=setwd(wd_0)
    )
    if (remove_old) {                
      ## TODO: add
    }
  })
  TRUE
  
  } 
)

#' @param repos \code{\link{RappRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepository
#' @export
setMethod(f = "refreshRepository", 
  signature = signature(
    repos = "RappRepositoryS3"
  ), 
  definition = function(
    repos,
    remove_old
  ) {
    
    refreshRepository(
      repos = expandRepository(path = repos),
      remove_old = remove_old
    )
        
  } 
)

