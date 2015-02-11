#' @title 
#' As repository
#'
#' @description 
#' Expands suitable objects pointing to package repositories in a way that 
#' all actual sub-level paths match the required structure and returns an 
#' object of class \code{\link[repor]{ExpandedRepository.S3}}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \code{\link{character}}.
#'    Path type. One of \code{ c("fs", "url_fs", "url_http", "url_ftp")}.
#' @param ensure \code{logical}.
#'    \code{TRUE}: ensure existence of repository (including sub-level pahts);
#'    \code{FALSE}: existence is not ensured
#' @param ... Further arguments passed to subsequent functions.
#'    In particular:
#'    \itemize{
#'      \item{\code{\link[repor]{ensureRepository}}}
#'    }
#' @example inst/examples/asRepository.r
#' @template author
#' @template references
#' @export
setGeneric(name="asRepository", 
  signature = c("repos"),
  def = function(
    repos = ".",
    type =  c("fs", "url_fs", "url_http", "url_ftp"),
    ensure = FALSE,
    ...
  ) {
  standardGeneric("asRepository")
})

#' @title 
#' As repository (miss)
#'
#' @description 
#' See generic: \code{\link{asRepository.S3}}.
#' 
#' @inheritParams asRepository
#' @param repos \code{\link{missing}}. 
#' @example inst/examples/asRepository.r
#' @template author
#' @template references
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
  asRepository(
    repos = repos,
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @title 
#' As repository (char)
#'
#' @description 
#' See generic: \code{\link{asRepository.S3}}.
#' 
#' @inheritParams asRepository
#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/asRepository.r
#' @template author
#' @template references
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
  repos <- PackageRepository$new(root = repos, type = type)
  if (ensure) {
    repos$ensure()
  } 
  repos
  
  }
)

#' @title 
#' As repository (PackageRepository)
#'
#' @description 
#' See generic: \code{\link{asRepository.S3}}.
#' 
#' @inheritParams asRepository
#' @param repos \code{\link{PackageRepository}}. 
#' @return TODO 
#' @example inst/examples/asRepository.r
#' @template author
#' @template references
#' @export
setMethod(f = "asRepository", 
  signature = signature(
    repos = "PackageRepository"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
  if (ensure) {
    repos$ensure() 
  }    
  repos
  
  }
)