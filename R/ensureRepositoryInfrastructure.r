#' Ensure Repository Infrastructure
#'
#' @description 
#' Ensures repository infrastructure.
#' 
#' @param repos_home \strong{Signature argument}.
#'    Object containing information about the repository parent location.
#' @param pkg_name \strong{Signature argument}.
#'    Object containing package name information.
#' @param pkg_version \strong{Signature argument}.
#'    Object containing package version information.
#' @param ... Further arguments passed to 
#'    \code{\link[rapp.core.repos]{ensureRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/ensureRepositoryInfrastructure.R
#' @export ensureRepositoryInfrastructure
setGeneric(name="ensureRepositoryInfrastructure", 
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
  standardGeneric("ensureRepositoryInfrastructure")
})

#' @param repos_home \code{\link{character}}. 
#' @param pkg_name \code{\link{missing}}. 
#' @param pkg_version \code{\link{missing}}. 
#' @return TODO 
#' @describeIn ensureRepositoryInfrastructure
#' @import rapp.core.description
#' @export
setMethod(f = "ensureRepositoryInfrastructure", 
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
  
  ensureRepositoryInfrastructure(
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
#' @describeIn ensureRepositoryInfrastructure
#' @import rapp.core.description
#' @export
setMethod(f = "ensureRepositoryInfrastructure", 
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
  tmp <- file.path(repos_home, repos_tree)
  names(tmp) <- repos_tree
  
  if (length(pkg_name) && length(pkg_version)) {
    tmp["packages"] <- file.path(tmp["packages"], pkg_name, pkg_version)
  } else {
    tmp <- tmp[-which(names(tmp) == "packages")]
  }
  
  sapply(unname(tmp), ensureRepository, USE.NAMES = FALSE)
    
  } 
)
