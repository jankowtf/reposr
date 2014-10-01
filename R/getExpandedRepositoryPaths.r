#' Get Repository Scaffold
#'
#' @description 
#' Provides scaffold of a package repository as expected by R.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param type \code{\link{character}}.
#'    Path type. One of \code{ c("fs", "url_file", "url_http", "url_ftp")}.
#'    See \code{\link[repositr]{normalizeRepositoryPath}}.
#' @param ... Further arguments passed to:
#'    \code{\link[repositr]{getPartialRepositoryScaffold}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/getExpandedRepositoryPaths.R
#' @seealso \code{\link[rapp2]{getExpandedRepositoryPaths-character-method}}
#' @export getExpandedRepositoryPaths
setGeneric(name="getExpandedRepositoryPaths", 
  signature = c("repos"),
  def = function(
    repos = asRepository("."),
    type =  c("fs", "url_file", "url_http", "url_ftp"),
    ...
  ) {
  standardGeneric("getExpandedRepositoryPaths")
})

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return \code{RappExpandedPackageRepositoryS3}. Object containing all
#'    repository sublevels with respective class attributes. 
#' @describeIn getExpandedRepositoryPaths
#' @export
setMethod(f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
      
  partial <- getPartialRepositoryScaffold(...)
  ## ...: 'rversion'
  
  repos <- normalizeRepositoryPath(repos = repos, type = type)
  out <- as.list(file.path(repos, partial))
  names(out) <- names(partial)
  
#   for (ii in names(out)) {
#     out[[ii]] <- addClassAttribute(
#       out[[ii]], 
#       class_name = switch(ii,
#         "mac.binary" = "RappPackageRepositoryMacBinaryS3",
#         "source" = "RappPackageRepositorySourceS3",
#         "win.binary" = "RappPackageRepositoryWinBinaryS3"
#       )
#     )
#   }
  out[sort(names(out))]
  
  } 
)

#' @param repos \code{\link{missing}}. 
#' @describeIn getExpandedRepositoryPaths
#' @export
setMethod(f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  getExpandedRepositoryPaths(
    repos=repos,
    type,
    ...
  )
    
  } 
)

#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @describeIn getExpandedRepositoryPaths
#' @export
setMethod(f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  getExpandedRepositoryPaths(
    repos = asRepository(repos = repos),
    type = type,
    ...
  )
    
  } 
)
