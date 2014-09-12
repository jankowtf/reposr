#' Remove Old Packages From Repository
#'
#' @description 
#' Removes outdated package builds from a repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \code{character}.
#'    Package types to be removed. Any subset of \code{c("mac.binary", 
#'    "source", "win.binary")}. If missing, \strong{all} package types are 
#'    considered.
#' @param ... Further arguments passed to:
#'    \code{\link[rapp.core.repos]{asExpandedRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/removeOldRepositoryPackages.R
#' @export removeOldRepositoryPackages
setGeneric(name="removeOldRepositoryPackages", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    type = c("mac.binary", "source", "win.binary"),
    ...
  ) {
  standardGeneric("removeOldRepositoryPackages")
})

#' @param repos \code{\link{character}}. 
#' @return \code{\link{logical}}. 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  return(removeOldRepositoryPackages(
    repos = asRepository(repos = repos),
    type = type
  ))
        
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  return(removeOldRepositoryPackages(
    repos = asExpandedRepository(repos = repos, ...), 
    type = type
  ))
        
  } 
)



#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
  
  type <- match.arg(type, c("mac.binary", "source", "win.binary"), 
                    several.ok = TRUE)
  repos <- repos[type]
  
  return(sapply(repos, function(ii) {
    removeOldRepositoryPackages(repos = ii)
  }))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}. 
#' @return TODO 
#' @describeIn removeOldRepositoryPackages
#' @export
#' @import rapp.core.condition
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos,
    type,
    pattern = character(),
    ...
  ) {
  
  pattern <- unlist(pattern)
  
  if (!file.exists(getRepositoryRoot(repos = repos))) {
    rapp.core.condition::signalCondition(
      condition = "InvalidPackageRepositoryLocation",
      msg = c(
        "Package repository directory does not exist",
        Path = repos
      ),
      type = "error"
    )
  }    
  
  if (!is.na(pattern) && length(pattern)) {
    files <- list.files(repos, full.names = TRUE)
    idx_keep <- unlist(lapply(c("PACKAGES", pattern), grep, files))
    
    if (length(idx_keep)) {
      files <- files[-idx_keep]
    }
    if (length(files)) {
      sapply(files, unlink, force = TRUE)
    }
  }
  TRUE
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return TODO 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
  
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- removeOldRepositoryPackages(
    repos = repos, 
    pattern = getPackageBuildFilePatterns(input = repos)
  )
  out
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return TODO 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- removeOldRepositoryPackages(
    repos = repos, 
    pattern = getPackageBuildFilePatterns(input = repos)    
  )
  out
  
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @return TODO 
#' @describeIn removeOldRepositoryPackages
#' @export
setMethod(f = "removeOldRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- removeOldRepositoryPackages(
    repos = repos, 
    pattern = getPackageBuildFilePatterns(input = repos)    
  )
  out
  
  } 
)



