#' Get Repository Infrastructure Paths
#'
#' @description 
#' Get repository infrastructure paths.
#' 
#' @param repos_home \strong{Signature argument}.
#'    Object containing information about the repository parent location.
#' @param pkg_name \strong{Signature argument}.
#'    Object containing package name information.
#' @param pkg_version \strong{Signature argument}.
#'    Object containing package version information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @example inst/examples/getRepositoryInfrastructurePaths.R
#' @export getRepositoryInfrastructurePaths
setGeneric(name="getRepositoryInfrastructurePaths", 
  signature = c(
    "repos_home",
    "pkg_name",
    "pkg_version"
  ),
  def = function(
    repos_home = file.path(Sys.getenv("HOME"), ".rapp/repos"),
    pkg_name = character(),
    pkg_version = character(),
    ...
  ) {
  standardGeneric("getRepositoryInfrastructurePaths")
})

#' @param repos_home \code{\link{character}}. 
#' @param pkg_name \code{\link{missing}}. 
#' @param pkg_version \code{\link{missing}}. 
#' @return TODO 
#' @describeIn getRepositoryInfrastructurePaths
#' @import descriptionr
#' @export
setMethod(f = "getRepositoryInfrastructurePaths", 
  signature = signature(
    repos_home = "character",
    pkg_name = "missing",
    pkg_version = "missing"
  ), 
  definition = function(
    repos_home,
    pkg_name,
    pkg_version,
    ...
  ) {
  
  if (file.exists("DESCRIPTION")) {
  ## Automatic creation of package project specific repository 
  ## only makes sense if the method is called from within an actual
  ## project. 
    pkg_name <- getPackageName()
    pkg_version <- getPackageVersion()
  } else {
    pkg_name <- character()
    pkg_version <- character()
  }
  
  getRepositoryInfrastructurePaths(
    repos_home = repos_home,
    pkg_name = pkg_name,
    pkg_version = pkg_version
  )
  
  } 
)

#' @param repos_home \code{\link{character}}. 
#' @param pkg_name \code{\link{character}}. 
#' @param pkg_version \code{\link{character}}. 
#' @return TODO 
#' @describeIn getRepositoryInfrastructurePaths
#' @import descriptionr
#' @export
setMethod(f = "getRepositoryInfrastructurePaths", 
  signature = signature(
    repos_home = "character",
    pkg_name = "character",
    pkg_version = "character"
  ), 
  definition = function(
    repos_home,
    pkg_name,
    pkg_version,
    ...
  ) {
    
  repos_tree <- c("global", "packages")
  out <- file.path(repos_home, repos_tree)
  names(out) <- repos_tree
  
  if (length(pkg_name) && length(pkg_version)) {
    out["packages"] <- file.path(out["packages"], pkg_name, pkg_version)
  } else {
    out <- out[-which(names(out) == "packages")]
  }
  
  out
    
  } 
)
