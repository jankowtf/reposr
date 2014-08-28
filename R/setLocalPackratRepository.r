#' Set Local Packrat Repository
#'
#' @description 
#' Set local packrat package repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository path information.
#' @param append_existing \code{\link{logical}}.
#'    Append value of \code{repos} to existing values of 
#'    \code{packrat::get_opts("local.repos")} (\code{TRUE}, default) or overwrite 
#'    existing content (\code{FALSE})
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/setLocalPackratRepository.R
#' @export setLocalPackratRepository
setGeneric(name="setLocalPackratRepository", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    append_existing = TRUE
  ) {
    standardGeneric("setLocalPackratRepository")
  })

#' @param repos \code{\link{character}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  return(setLocalPackratRepository(
    repos = asRepository(repos),
    append_existing = append_existing
  ))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  #   if (!"packrat" %in% .packages()) {
#     signalCondition(
#       condition = "PackratPackageNotLoaded",
#       type = "error"
#     )
#   }
  
  if (!append_existing) {
    packrat::set_opts(local.repos = repos)  
  } else {
    repos_old <- packrat::get_opts("local.repos")
    packrat::set_opts(local.repos = unique(c(repos_old, repos)))  
  }
  TRUE
  
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  return(setLocalPackratRepository(
    repos = asRepository(repos = repos),
    append_existing = append_existing
  ))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  return(setLocalPackratRepository(
    repos = asRepository(repos = repos),
    append_existing = append_existing
  ))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  return(setLocalPackratRepository(
    repos = asRepository(repos = repos),
    append_existing = append_existing
  ))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn setLocalPackratRepository
#' @export
setMethod(f = "setLocalPackratRepository", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos,
    append_existing
  ) {
    
  return(setLocalPackratRepository(
    repos = asRepository(repos = repos),
    append_existing = append_existing
  ))
  
  } 
)
