#' As Expanded Repository
#'
#' @description 
#' Expands actual repository paths and returns an object of class 
#' \code{\link[repositr]{RappExpandedRepositoryS3}}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param ensure \code{logical}.
#'    Ensure repository existence (\code{TRUE}) or not (\code{FALSE}, default).
#' @param type \code{\link{character}}.
#'    Path type. One of \code{ c("fs", "url_file", "url_http", "url_ftp")}.
#'    See \code{\link[repositr]{normalizeRepositoryPath}}.
#' @param ... Further arguments passed to:
#'    \code{\link[repositr]{getPartialRepositoryScaffold}},
#'    \code{\link[repositr]{ensureRepository}}..
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/asExpandedRepository.R
#' @export asExpandedRepository
setGeneric(name="asExpandedRepository", 
  signature = c("repos"),
  def = function(
    repos = asRepository("."),
    ensure = FALSE,
    type =  c("fs", "url_file", "url_http", "url_ftp"),
    ...
  ) {
  standardGeneric("asExpandedRepository")
})

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return \code{RappExpandedPackageRepositoryS3}. Object containing all
#'    repository sublevels with respective class attributes 
#'    (\code{RappPackageRepositoryMacBinaryS3}, 
#'    \code{RappPackageRepositoryWinBinaryS3},
#'    \code{RappPackageRepositorySourceS3}.
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
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
    
  ## Expand paths //
  out <- getExpandedRepositoryPaths(repos = repos, type = type, ...)
  
  for (ii in names(out)) {
    out[[ii]] <- addClassAttribute(
      out[[ii]], 
      class_name = switch(ii,
        "mac.binary" = "RappPackageRepositoryMacBinaryS3",
        "source" = "RappPackageRepositorySourceS3",
        "win.binary" = "RappPackageRepositoryWinBinaryS3"
      )
    )
  }
  
  out <- addClassAttribute(obj = out, 
    class_name = "RappExpandedPackageRepositoryS3")
  out[sort(names(out))]
  
  } 
)

#' @param repos \code{\link{missing}}. 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    ensure,
    type,
    ...
  ) {
    
  asExpandedRepository(
    repos = repos,
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ensure, 
    type,
    ...
  ) {
    
#   repos <- normalizeRepositoryPath(repos = repos, type = "fs")        
  asExpandedRepository(
    repos = asRepository(repos = repos, type = type),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
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
  for (ii in seq(along = repos)) {
    repos[[ii]] <- normalizeRepositoryPath(repos = unclass(repos[[ii]]), type = type)
  }
  repos
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos,
    ensure, 
    type,
    ...
  ) {
   
  ## Dispatch to 'repos:RappPackageRepositoryS3' //
  asExpandedRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos,
    ensure, 
    type,
    ...
  ) {
   
  ## Dispatch to 'repos:RappPackageRepositoryS3' //
  asExpandedRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos,
    ensure, 
    type,
    ...
  ) {
   
  ## Dispatch to 'repos:RappPackageRepositoryS3' //
  asExpandedRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @return TODO 
#' @describeIn asExpandedRepository
#' @export
setMethod(f = "asExpandedRepository", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos,
    ensure, 
    type,
    ...
  ) {
   
  ## Dispatch to 'repos:RappPackageRepositoryS3' //
  asExpandedRepository(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type,
    ...
  )
    
  } 
)
