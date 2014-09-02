#' As Repository
#'
#' @description 
#' Turns a \code{character} path of a package repository into an object 
#' of class \code{RappPackageRepositoryS3}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param ensure \code{logical}.
#'    Ensure repository existence (\code{TRUE}) or not (\code{FALSE}, default).
#' @param ... Further arguments passed to:
#'    \code{\link[rapp.core.repos]{ensureRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/asRepository.R
#' @export asRepository
setGeneric(name="asRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = ".",
    ensure = FALSE,
    ...
  ) {
  standardGeneric("asRepository")
})

#' @param repos \code{\link{character}}.  
#' @return \code{RappPackageRepositoryS3}. Identical to \code{repos} with updated
#' 		class table. 
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
    
  return(asRepository(
    repos = addClassAttribute(obj = repos, 
      class_name = "RappPackageRepositoryS3"),
    ensure = ensure,
    ...
  ))
  
  } 
)

#' @param repos \code{\link{missing}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
    
  return(asRepository(
    repos = repos,
    ensure = ensure,
    ...
  ))
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  if (ensure) {
    ensureRepository(repos = repos, ...)
    ## ...: 'rversion'
  }    
  repos
    
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  ## Keep for reference //
#   patterns <- getPartialRepositoryScaffold(rversion = rversion)
#   repos <- unique(sapply(names(repos), function(ii) {
#     gsub(paste0("/", patterns[[ii]]), "", repos[[ii]])
#   }))
    
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}.  
#' @describeIn asRepository
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos,
    ensure,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    ...
  )
    
  } 
)

