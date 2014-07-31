#' As Expanded Repository
#'
#' @description 
#' Turns a \code{character} path of a package repository into an object 
#' of class \code{RappExpandedRepositoryS3}.
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/asExpandedRepository.R
#' @export asExpandedRepository
setGeneric(name="asExpandedRepository", 
  signature = c(
    "path"
  ),
  def = function(
      path="."
  ) {
  standardGeneric("asExpandedRepository")
})

#' @param path \code{\link{character}}.  
#' @return \code{RappRepositoryS3}. Identical to \code{path} with updated
#' 		class table. 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
      path = "character"
  ), 
  definition = function(
      path
  ) {
      
  class(path) <- c("RappExpandedRepositoryS3", class(path))
  path
  
  } 
)

#' @param path \code{\link{missing}}.  
#' @return See signature \code{character}.
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    path = "missing"
  ), 
  definition = function(
    path
  ) {
    
    asExpandedRepository(
      path = path
    )
    
  } 
)

