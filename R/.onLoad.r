#' On Load Hook
#'
#' @description 
#' On load hook.
#' 
#' @param libname 
#' @param pkgname
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @export .onLoad
.onLoad <- function(libname, pkgname) {
  setOldClass("PackageRepository.S3")
  setOldClass("PackageRepositoryRoot.S3")
  setOldClass("PackageRepositorySubGeneric.S3")
  setOldClass("PackageRepositorySubMac.S3")
  setOldClass("PackageRepositorySubMac.S3")
  setOldClass("PackageRepositorySubSource.S3")
  setOldClass("RappPackageRepositoryListS3")
  setOldClass("RappParsedPackageRepositoryIndexS3")
}
