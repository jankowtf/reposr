#' As Expanded Repository
#'
#' @description 
#' Turns a \code{character} path of a package repository into an object 
#' of class \code{PackageRepository.S3}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param ensure \code{logical}.
#'    Ensure existence (\code{TRUE}) or not (\code{FALSE}, default).
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @example inst/examples/asRepository.R
#' @export asRepository
setGeneric(name="asRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = ".",
    ensure = FALSE
  ) {
  standardGeneric("asRepository")
})

#' @param repos \code{\link{character}}.  
#' @return \code{PackageRepositoryRoot.S3}. Identical to \code{repos} with updated
#' 		class table. 
#' @describeIn asRepository
#' @export
#' @import conditionr
setMethod(f = "asRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ensure
  ) {
  
  if (!length(repos)) {
    conditionr::signalCondition(
      condition = "InvalidRepositoryPath",
      msg = c(
        "Invalid repository path: empty character string"
      ),
      type = "error"
    )
  }    
  if (length(repos) > 1) {
    conditionr::signalCondition(
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
    class(repos) <- unique(c("PackageRepository.S3", class(repos)))
  }
  
  if (ensure) {
    rversion <- basename(repos["win.binary"])
    ensureRepository(
      repos = asRepositoryRoot(repos = repos, rversion = rversion), 
      rversion = rversion
    )
  }
  
  repos
  
  } 
)

#' @param repos \code{\link{missing}}.  
#' @return See signature \code{character}.
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos
  ) {
    
    asRepository(
      repos = repos
    )
    
  } 
)

