#' Ensure Repository
#'
#' @description 
#' Ensures existence of an R package repository.
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @param rversion \strong{Signature argument}.
#'    Object containing version information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/ensureRepository.R
#' @export ensureRepository
setGeneric(name="ensureRepository", 
  signature = c(
    "path",
    "rversion"
  ),
  def = function(
      path=".",
      rversion=paste(R.version$major, 
        unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
  ) {
  standardGeneric("ensureRepository")
})

#' @param path \code{\link{RappRepositoryS3}}. 
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
      path = "RappRepositoryS3",
      rversion = "character"
  ), 
  definition = function(
      path,
      rversion
  ) {
      
  repos <- expandRepository(path = path, rversion=rversion)
  sapply(repos, dir.create, recursive=TRUE, showWarnings=FALSE)
  ensureRepositoryRegistryFiles(repos = repos)
  out <- TRUE
  names(out) <- path
  out
  
  } 
)

#' @param path \code{\link{RappRepositoryS3}}. 
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    path = "RappRepositoryS3",
    rversion = "missing"
  ), 
  definition = function(
    path,
    rversion
  ) {
    
    ensureRepository(
      path=path,
      rversion=rversion
    )
    
  } 
)

