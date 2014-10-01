#' Ensure Repository
#'
#' @description 
#' Ensures existence of an R package repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @param rversion \strong{Signature argument}.
#'    Object containing version information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/ensureRepository.R
#' @export ensureRepository
setGeneric(name="ensureRepository", 
  signature = c(
    "repos",
    "rversion"
  ),
  def = function(
    repos = ".",
    rversion=paste(R.version$major, 
      unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
  ) {
  standardGeneric("ensureRepository")
  }
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
      repos = "RappPackageRepositoryS3",
      rversion = "character"
  ), 
  definition = function(
      repos,
      rversion
  ) {

  out <- ensureRepository(
    repos = asExpandedRepository(repos = repos, rversion=rversion)
  )
  names(out) <- repos
  out
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    repos = "RappPackageRepositoryS3",
    rversion = "missing"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = repos,
    rversion = rversion
  )
    
  } 
)

#' @param repos \code{\link{character}}. 
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    repos = "character",
    rversion = "missing"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = asRepository(repos, type = "fs"),
    rversion = rversion
  )
    
  } 
)

#' @param repos \code{\link{character}}. 
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    repos = "character",
    rversion = "character"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = asRepository(repos, type = "fs"),
    rversion = rversion
  )
    
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @param rversion \code{\link{ANY}}. 
#' @return TODO 
#' @describeIn ensureRepository
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3",
    rversion = "ANY"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  sapply(repos, dir.create, recursive=TRUE, showWarnings=FALSE)
  ensureRepositoryIndex(repos = repos)
  TRUE
  
  } 
)

