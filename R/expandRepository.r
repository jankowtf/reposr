#' Get Repository Scaffold
#'
#' @description 
#' Provides scaffold of a package repository as expected by R.
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @param ... Further arguments passed to:
#'    \code{\link{getPartialRepositoryScaffold}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/expandRepository.R
#' @seealso \code{\link[rapp2]{expandRepository-character-method}}
#' @export expandRepository
setGeneric(name="expandRepository", 
  signature = c("path"),
  def = function(
      path=asRepository("."),
      ...
  ) {
  standardGeneric("expandRepository")
})

#' @param path \code{\link{RappRepositoryS3}}. 
#' @return TODO 
#' @describeIn expandRepository
#' @export
setMethod(f = "expandRepository", 
  signature = signature(
    path = "RappRepositoryS3"
  ), 
  definition = function(
    path,
    ...
  ) {
      
  partial <- getPartialRepositoryScaffold(...)
  out <- file.path(path, partial)
  class(out) <- c("RappExpandedRepositoryS3", class(out))
  out
  
  } 
)

#' @param path \code{\link{missing}}. 
#' @return TODO 
#' @describeIn expandRepository
#' @export
setMethod(f = "expandRepository", 
  signature = signature(
    path = "missing"
  ), 
  definition = function(
    path,
    ...
  ) {
    
  expandRepository(
    path=path,
    ...
  )
    
  } 
)

#' @param path \code{\link{character}}. 
#' @return TODO 
#' @describeIn expandRepository
#' @export
setMethod(f = "expandRepository", 
  signature = signature(
    path = "character"
  ), 
  definition = function(
    path,
    ...
  ) {
    
    expandRepository(
      path=asRepository(path = path),
      ...
    )
    
  } 
)
