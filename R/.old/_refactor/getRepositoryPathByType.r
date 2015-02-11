#' Select Repository Path By Type
#'
#' @description 
#' Selects repository path based on tyype.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \strong{Signature argument}.
#'    Object containing repository type information. 
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @example inst/examples/getRepositoryPathByType.R
#' @export getRepositoryPathByType
setGeneric(name="getRepositoryPathByType", 
  signature = c(
    "repos",
    "type"
  ),
  def = function(
      repos,
      type = c("win.binary", "source", "mac.binary")
  ) {
  standardGeneric("getRepositoryPathByType")
})

#' @param repos \code{\link{PackageRepository.S3}}.
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Repository path 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepository.S3",
      type = "character"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  type <- match.arg(type, c("win.binary", "source", "mac.binary"))
    
#  patterns <- c("macosx", "windows", "src")
#  is_detailed <- all(mapply(grepl, patterns, repos))
#  if (!is_detailed) {
#    repos <- asRepository(repos = repos)
#  }
  types <- getRepositorySubType(repos = repos)
  out <- unclass(repos[which(types %in% type)])
  out 

  } 
)

#' @param repos \code{\link{PackageRepository.S3}}.
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Repository path 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
    repos = "PackageRepository.S3",
    type = "missing"
  ), 
  definition = function(
    repos,
    type
  ) {
  
  getRepositoryPathByType(
    repos = repos, 
    type = type
  )    
      
  } 
)

#' @param repos \code{\link{PackageRepositoryRoot.S3}}.
#' @param type \code{\link{character}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepositoryRoot.S3",
      type = "character"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  getRepositoryPathByType(
    repos = asRepository(repos = repos), 
    type = type
  )
  
  } 
)

#' @param repos \code{\link{character}}.
#' @param type \code{\link{character}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "character",
      type = "character"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  getRepositoryPathByType(
    repos = asRepository(repos = repos), 
    type = type
  )
  
  } 
)

#' @param repos \code{\link{character}}.
#' @param type \code{\link{missing}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "character",
      type = "missing"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  return(getRepositoryPathByType(
    repos = asRepository(repos = repos), 
    type = type
  ))
  
  } 
)

#' @param repos \code{\link{PackageRepositoryRoot.S3}}.
#' @param type \code{\link{missing}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepositoryRoot.S3",
      type = "missing"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  return(getRepositoryPathByType(
    repos = asRepository(repos = repos), 
    type = type
  ))
  
  } 
)

#' @param repos \code{\link{PackageRepositorySubMac.S3}}.
#' @param type \code{\link{ANY}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepositorySubMac.S3",
      type = "ANY"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  return(repos)
  
  } 
)

#' @param repos \code{\link{PackageRepositorySubMac.S3}}.
#' @param type \code{\link{ANY}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepositorySubMac.S3",
      type = "ANY"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  return(repos)
  
  } 
)

#' @param repos \code{\link{PackageRepositorySubSource.S3}}.
#' @param type \code{\link{ANY}}. 
#' @describeIn getRepositoryPathByType
#' @export
setMethod(f = "getRepositoryPathByType", 
  signature = signature(
      repos = "PackageRepositorySubSource.S3",
      type = "ANY"
  ), 
  definition = function(
      repos,
      type
  ) {
  
  return(repos)
  
  } 
)
