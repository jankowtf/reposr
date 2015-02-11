#' @title 
#' Get expanded repository paths
#'
#' @description 
#' Creates the expected sub-level paths below a package repository's root path 
#' as expected for valid package repositories and appends them to the
#' repository's root path.
#' 
#' @details
#' Currently, this means that the following sub-level paths are created and 
#' appended to the repository's root path:
#' \itemize{
#'    \item{~/bin/macosx/contrib/<rversion>}
#'    \item{~/src/contrib}
#'    \item{~/bin/windows/contrib/<rversion>}
#' }
#' Note that the order of the sub-level paths is based on the \strong{names}
#' that are assigned to the sub-level paths: \code{mac.binary}, \code{source}
#' and \code{win.binary}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \code{\link{character}}.
#'    Path type. One of \code{ c("fs", "url_fs", "url_http", "url_ftp")}.
#'    See \code{\link[repor]{normalizeRepositoryPath}}.
#' @param ... Further arguments passed to subsequent functions.
#'    In particular:
#'    \itemize{
#'      \item{\code{\link[repor]{getRelativeRepositorySubPaths}}}
#'    }
#' @example inst/examples/getExpandedRepositoryPaths.r
#' @template author
#' @template references
#' @seealso \code{\link[rapp2]{getExpandedRepositoryPaths-character-method}}
#' @export getExpandedRepositoryPaths
setGeneric(
  name = "getExpandedRepositoryPaths", 
  signature = c("repos"),
  def = function(
    repos = asRepositoryRoot("."),
    type =  c("fs", "url_fs", "url_http", "url_ftp"),
    ...
  ) {
  standardGeneric("getExpandedRepositoryPaths")
})

#' @title 
#' Get expanded repository paths
#'
#' @description 
#' See generic: \code{\link{getExpandedRepositoryPaths}}
#' 
#' @inheritParams getExpandedRepositoryPaths
#' @param repos \code{\link{missing}}. 
#' @example inst/examples/getExpandedRepositoryPaths.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  getExpandedRepositoryPaths(
    repos=repos,
    type,
    ...
  )
    
  } 
)

#' @title 
#' Get expanded repository paths
#'
#' @description 
#' See generic: \code{\link{getExpandedRepositoryPaths}}
#' 
#' @inheritParams getExpandedRepositoryPaths
#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/getExpandedRepositoryPaths.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  getExpandedRepositoryPaths(
    repos = asRepositoryRoot(repos = repos),
    type = type,
    ...
  )
    
  } 
)

#' @title 
#' Get expanded repository paths
#'
#' @description 
#' See generic: \code{\link{getExpandedRepositoryPaths}}
#' 
#' @inheritParams getExpandedRepositoryPaths
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @return \code{PackageRepository.S3}. Object containing all
#'    repository sublevels with respective class attributes. 
#' @example inst/examples/getExpandedRepositoryPaths.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getExpandedRepositoryPaths", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
    
  partial <- getRelativeRepositorySubPaths(...)
  ## ...: 'rversion'
  
  repos <- normalizeRepositoryPath(repos = repos, type = type)
  out <- as.list(gsub("//", "/", file.path(repos$path, partial)))
  names(out) <- names(partial)
  
  #   for (ii in names(out)) {
  #     out[[ii]] <- addClassAttribute(
  #       out[[ii]], 
  #       class_name = switch(ii,
  #         "mac.binary" = "PackageRepositorySubMac.S3",
  #         "source" = "PackageRepositorySubSource.S3",
  #         "win.binary" = "PackageRepositorySubMac.S3"
  #       )
  #     )
  #   }
  out[sort(names(out))]
    
  } 
)