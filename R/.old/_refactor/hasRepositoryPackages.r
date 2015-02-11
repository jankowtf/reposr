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
#' 		\code{\link[repor]{asRepositoryRoot}},
#' 		\code{\link[repor]{asRepository}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
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
    repos = asRepositoryRoot(repos = repos, ...),
    ...
  )

  }
)

#' @param repos  \code{\link{PackageRepositoryRoot.S3}}. 
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
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

#' @param repos  \code{\link{PackageRepository.S3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepository.S3"
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

#' @param repos  \code{\link{PackageRepositorySubGeneric.S3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepositorySubGeneric.S3"
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

#' @param repos  \code{\link{PackageRepositorySubMac.S3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "PackageRepositorySubGeneric.S3"),
    ...
  ))
  
  }
)

#' @param repos  \code{\link{PackageRepositorySubMac.S3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "PackageRepositorySubGeneric.S3"),
    ...
  ))
  
  }
)

#' @param repos  \code{\link{PackageRepositorySubSource.S3}}. 
#' @return \code{logical}. For each package type, \code{TRUE} if any packages 
#'    exist in the respective sub repository,\code{FALSE} if not.
#' @describeIn hasRepositoryPackages
#' @export
setMethod(f = "hasRepositoryPackages", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition=function(
    repos,
    ...
  ) {

  return(hasRepositoryPackages(
    repos = addClassAttribute(obj = repos, 
      class_name = "PackageRepositorySubGeneric.S3"),
    ...
  ))
  
  }
)
