#' Select Repository Path By Type
#'
#' @description 
#' Selects repository path based on tyype.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \strong{Signature argument}.
#'    Object containing repository type information. 
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/getRepositoryPathByType.R
#' @export getRepositoryPathByType
setGeneric(name="getRepositoryPathByType", 
  signature = c(
    "repos",
    "type"
  ),
  def = function(
      repos,
      type = c("win.binary", "source", "mac.binary")
  ) {
  standardGeneric("getRepositoryPathByType")
})

#' @param repos \code{\link{RappExpandedRepositoryS3}}.
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Repository path 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "RappExpandedRepositoryS3",
      type = "character"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  type <- match.arg(type, c("win.binary", "source", "mac.binary"))
    
#  patterns <- c("macosx", "windows", "src")
#  is_detailed <- all(mapply(grepl, patterns, repos))
#  if (!is_detailed) {
#    repos <- expandRepository(path = repos)
#  }
  types <- getRepositoryType(repos = repos)
  repos[which(types %in% type)]  
 
  } 
)

#' @param repos \code{\link{RappExpandedRepositoryS3}}.
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Repository path 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
    repos = "RappExpandedRepositoryS3",
    type = "missing"
  ), 
  definition = function(
    repos,
    type
  ) {
  
  getRepositoryPathByType(
    repos = repos, 
    type = type
  )    
      
  } 
)

