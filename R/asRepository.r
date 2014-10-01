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
#' @param type \code{\link{character}}.
#'    Path type. One of \code{ c("fs", "url_file", "url_http", "url_ftp")}.
#'    See \code{\link[repositr]{normalizeRepositoryPath}}.
#' @param ... Further arguments passed to:
#'    \code{\link[repositr]{ensureRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/asRepository.R
#' @seealso \code{
#'    \link[repositr]{normalizeRepositoryPath}
#' }
#' @export asRepository
setGeneric(name="asRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = ".",
    ensure = FALSE,
    type =  c("fs", "url_file", "url_http", "url_ftp"),
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
    type, 
    ...
  ) {
    
  repos <- normalizeRepositoryPath(repos = repos, type = type)        
  return(asRepository(
    repos = addClassAttribute(obj = repos, 
      class_name = "RappPackageRepositoryS3"),
    ensure = ensure,
    type = type,
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
    type,
    ...
  ) {
    
  return(asRepository(
    repos = repos,
    ensure = ensure,
    type = type,
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
    type,
    ...
  ) {
  
  if (ensure) {
    ensureRepository(repos = repos, ...)
    ## ...: 'rversion'
  }    
  tmp <- normalizeRepositoryPath(repos = unclass(repos), type = type, ...)
  repos[1:length(repos)] <- tmp
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
    type,
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
    type = type,
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
    type,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
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
    type,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
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
    type,
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
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
    type, 
    ...
  ) {
  
  ## Dispatch to 'RappPackageRepositoryS3' //
  asRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

