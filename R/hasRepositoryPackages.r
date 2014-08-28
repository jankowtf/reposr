#' Exist Packages In Repository
#'
#' @description 
#' Checks if a repository, or to be more precise, the actual sublevel repositories
#' (for binary and source builds), contain any packages at all by investigating 
#' its index files (\code{PACKAGES}).
#' 
#' @param repos \strong{Signature argument}.
#' 		Object containing repository information.
#' @param further arguments passed to:
#' 		\code{\link[rapp.core.repos]{asRepository}},
#' 		\code{\link[rapp.core.repos]{asExpandedRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/hasRepositoryPackages.R
#' @export 
setGeneric(name = "hasRepositoryPackages", 
  signature = c(
    "repos"
  ),
  def=function(
    repos = ".",
    ...
  ) {
  standardGeneric("hasRepositoryPackages")
})

#' @param repos  \code{\link{missing}}. 
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f="hasRepositoryPackages", 
  signature=signature(
    repos = "missing"
  ), 
  definition=function(
    repos,
    ...
  ) {

  hasRepositoryPackages(
    repos = repos,
    ...
  )
      
  }
)

#' @param repos  \code{\link{character}}. 
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "character"
  ), 
  definition=function(
    repos,
    ...
  ) {
      
  hasRepositoryPackages(
    repos = asRepository(repos = repos, ...),
    ...
  )

  }
)

#' @param repos  \code{\link{RappPackageRepositoryS3}}. 
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition=function(
    repos,
    ...
  ) {
      
  hasRepositoryPackages(
    repos = asExpandedRepository(repos = repos, ...),
    ...
  )

  }
)

#' @param repos  \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition=function(
    repos,
    ...
  ) {
  
#   refreshRepositoryIndex(repos)
  return(unlist(unname(lapply(repos, function(ii) {
    hasRepositoryPackages(repos = ii, ...)
  }))))
  
  }
)

#' @param repos  \code{\link{RappPackageRepositoryGenericS3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  index_file <- file.path(repos, "PACKAGES")
  out <- length(parseRepositoryIndexFile(path = index_file)) != 0
  names(out) <- repos
  out

  }
)

#' @param repos  \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "RappPackageRepositoryGenericS3"),
    ...
  ))
  
  }
)

#' @param repos  \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "RappPackageRepositoryGenericS3"),
    ...
  ))
  
  }
)

#' @param repos  \code{\link{RappPackageRepositorySourceS3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "RappPackageRepositoryGenericS3"),
    ...
  ))
  
  }
)
