#' As Repository
#'
#' @description 
#' Turns a \code{character} path of a package repository into an object 
#' of class \code{RappRepositoryS3}.
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/asRepository.R
#' @export asRepository
setGeneric(name="asRepository", 
  signature = c(
    "path"
  ),
  def = function(
      path="."
  ) {
  standardGeneric("asRepository")
})

#' @param path \code{\link{character}}.  
#' @return \code{RappRepositoryS3}. Identical to \code{path} with updated
#' 		class table. 
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
      path = "character"
  ), 
  definition = function(
      path
  ) {
      
  
  class(path) <- c("RappRepositoryS3", class(path))
  path
  
  } 
)

#' @param path \code{\link{missing}}.  
#' @return See signature \code{character}.
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    path = "missing"
  ), 
  definition = function(
    path
  ) {
    
    asRepository(
      path = path
    )
    
  } 
)

