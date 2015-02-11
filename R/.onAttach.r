#' On Attach Hook
#'
#' @description 
#' On Attach hook.
#' 
#' @param libname 
#' @param pkgname
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @export .onAttach
.onAttach <- function(libname, pkgname) {
  setOldClass("PackageRepository.S3")
  setOldClass("PackageRepositoryRoot.S3")
}
