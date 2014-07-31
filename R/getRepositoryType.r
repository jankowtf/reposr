#' Get Repository Type
#'
#' @description 
#' Infers the repository type based on repository location.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/getRepositoryType.R
#' @export getRepositoryType
setGeneric(name="getRepositoryType", 
  signature = c(
    "repos"
  ),
  def = function(
      repos
  ) {
  standardGeneric("getRepositoryType")
})

#' @param repos \code{\link{RappExpandedRepositoryS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
      repos = "RappExpandedRepositoryS3"
  ), 
  definition = function(
      repos
  ) {
     
  tmp <- sapply(repos, function(ii) {
    basename(gsub("/contrib", "", dirname(ii)))
  })
  sapply(tmp, function(ii) {
    switch(
      ii,
      "src"="source",
      "macosx"="mac.binary",
      "windows"="win.binary"
    )
  })
  
  } 
)

#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos
  ) {
    
    getRepositoryType(
      repos = asExpandedRepository(path = repos)
    )
    
  } 
)

