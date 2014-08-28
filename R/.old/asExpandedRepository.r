#' As Expanded Repository
#'
#' @description 
#' Turns a \code{character} path of a package repository into an object 
#' of class \code{RappExpandedPackageRepositoryS3}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param ensure \code{logical}.
#'    Ensure existence (\code{TRUE}) or not (\code{FALSE}, default).
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/asExpandedRepository.R
#' @export asExpandedRepository
setGeneric(name="asExpandedRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = ".",
    ensure = FALSE
  ) {
  standardGeneric("asExpandedRepository")
})

#' @param repos \code{\link{character}}.  
#' @return \code{RappPackageRepositoryS3}. Identical to \code{repos} with updated
#' 		class table. 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ensure
  ) {
  
  if (!length(repos)) {
    signalCondition(
      condition = "InvalidRepositoryPath",
      msg = c(
        "Invalid repository path: empty character string"
      ),
      type = "error"
    )
  }    
  if (length(repos) > 1) {
    signalCondition(
      condition = "InvalidRepositoryPath",
      msg = c(
        "Invalid repository path: length > 1"
      ),
      type = "error"
    )
  }    
  type <- c("mac.binary", "source", "win.binary")
  if (!any(type %in% names(repos))) {
    repos <- getExpandedRepositoryPaths(repos = repos)  
  } else {
    class(repos) <- unique(c("RappExpandedPackageRepositoryS3", class(repos)))
  }
  
  if (ensure) {
    rversion <- basename(repos["win.binary"])
    ensureRepository(
      repos = asRepository(repos = repos, rversion = rversion), 
      rversion = rversion
    )
  }
  
  repos
  
  } 
)

#' @param repos \code{\link{missing}}.  
#' @return See signature \code{character}.
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos
  ) {
    
    asExpandedRepository(
      repos = repos
    )
    
  } 
)

