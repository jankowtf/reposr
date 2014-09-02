#' Get Partial Repository Scaffold
#'
#' @description 
#' Provides partial scaffold of a package repository as expected by R.
#' 
#' @param rversion \strong{Signature argument}.
#'    Object containing R version information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.packrat}
#' @example inst/examples/getPartialRepositoryScaffold.R
#' @seealso \code{\link[rapp2]{getPartialRepositoryScaffold-character-method}}
#' @export getPartialRepositoryScaffold
setGeneric(name="getPartialRepositoryScaffold", 
  signature = c("rversion"),
  def = function(
      rversion=paste(R.version$major, 
        unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
  ) {
  standardGeneric("getPartialRepositoryScaffold")
})

#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @describeIn getPartialRepositoryScaffold
#' @export
setMethod(f = "getPartialRepositoryScaffold", 
  signature = signature(
      rversion = "character"
  ), 
  definition = function(
      rversion
  ) {

  out <- sort(file.path(c(
    file.path("bin", file.path(c("macosx", "windows"), "contrib", rversion)),
    file.path("src", "contrib")
  )))
  names(out) <- c("mac.binary", "win.binary", "source")
  out
  
  } 
)

#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @describeIn getPartialRepositoryScaffold
#' @export
setMethod(f = "getPartialRepositoryScaffold", 
  signature = signature(
    rversion = "missing"
  ), 
  definition = function(
    rversion
  ) {
    
  getPartialRepositoryScaffold(
    rversion=rversion
  )
    
  } 
)

