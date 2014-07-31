#' On Load Hook
#'
#' @description 
#' On load hook.
#' 
#' @param libname 
#' @param pkgname
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @export .onLoad
.onLoad <- function(libname, pkgname) {
  setOldClass("RappExpandedRepositoryS3")
  setOldClass("RappRepositoryS3")
}
