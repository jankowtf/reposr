#' On Load Hook
#'
#' @description 
#' On load hook.
#' 
#' @param libname 
#' @param pkgname
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @export .onLoad
.onLoad <- function(libname, pkgname) {
  setOldClass("RappExpandedPackageRepositoryS3")
  setOldClass("RappPackageRepositoryS3")
  setOldClass("RappPackageRepositoryGenericS3")
  setOldClass("RappPackageRepositoryMacBinaryS3")
  setOldClass("RappPackageRepositoryWinBinaryS3")
  setOldClass("RappPackageRepositorySourceS3")
  setOldClass("RappPackageRepositoryListS3")
  setOldClass("RappParsedPackageRepositoryIndexS3")
}
