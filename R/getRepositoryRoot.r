#' Get Repository's Root Path
#'
#' @description 
#' Get a repository's root path.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repos information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/getRepositoryRoot.R
#' @export getRepositoryRoot
setGeneric(name="getRepositoryRoot", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = "."
  ) {
  standardGeneric("getRepositoryRoot")
})

#' @param repos \code{\link{character}}.  
#' @return \code{RappPackageRepositoryS3}. No matter what type repository has 
#'    been passed in, the method returns only the root part of the repository.
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos
  ) {
      
  return(asRepository(repos = repos))
  
  } 
)

#' @param repos \code{\link{missing}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos
  ) {
    
  return(getRepositoryRoot(repos = repos))
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos
  ) {
  
  return(repos)
    
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}.  
#' @describeIn getRepositoryRoot
#' @export
#' @import conditionr
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos
  ) {
  
  tmp <- unique(sapply(repos, getRepositoryRoot))
  if (length(tmp) > 1) {
    conditionr::signalCondition(
      condition = "Ambiguous package repository structure",
      msg = c(
        "Unable to identify unambiguous root repository",
        Found = paste(tmp, collapse = ", ")
      ),
      type = "error"
    )
  }
  return(asRepository(repos = tmp))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos
  ) {
  
  out <- asRepository(
    repos = unclass(gsub("(?<=repos)/.*", "", repos, perl = TRUE))
  )
  return(out)
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos
  ) {
  
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")    
  return(getRepositoryRoot(repos = repos))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos
  ) {
  
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")    
  return(getRepositoryRoot(repos = repos))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}.  
#' @describeIn getRepositoryRoot
#' @export
setMethod(f = "getRepositoryRoot", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos
  ) {
  
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")    
  return(getRepositoryRoot(repos = repos))
  
  } 
)

