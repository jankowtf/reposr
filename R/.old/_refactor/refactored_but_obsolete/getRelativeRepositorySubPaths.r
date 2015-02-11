#' @title 
#' Get partial repository scaffold
#'
#' @description 
#' Provides partial scaffold of a package repository as expected by R.
#' 
#' @param rversion \strong{Signature argument}.
#'    Object containing R version information.
#' @example inst/examples/getRelativeRepositorySubPaths.r
#' @template author
#' @template references
#' @seealso \code{\link[rapp2]{getRelativeRepositorySubPaths-character-method}}
#' @export getRelativeRepositorySubPaths
setGeneric(
  name = "getRelativeRepositorySubPaths", 
  signature = c("rversion"),
  def = function(
    rversion=paste(R.version$major, 
                   unlist(strsplit(R.version$minor, split="\\."))[2], sep=".")
  ) {
    standardGeneric("getRelativeRepositorySubPaths")
  })

#' @title 
#' Get partial repository scaffold
#'
#' @description 
#' See generic: \code{\link[repor]{getRelativeRepositorySubPaths}}.
#' 
#' @inheritParams getRelativeRepositorySubPaths
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/getRelativeRepositorySubPaths.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRelativeRepositorySubPaths", 
  signature = signature(
    rversion = "character"
  ), 
  definition = function(
    rversion
  ) {
    
  out <- sort(file.path(c(
    file.path("bin", file.path(c("macosx", "windows"), "contrib", rversion)),
    file.path("src", "contrib")
  )))
  names(out) <- c("mac.binary", "win.binary", "source")
  out
    
  } 
)

#' @title 
#' Get partial repository scaffold
#'
#' @description 
#' See generic: \code{\link[repor]{getRelativeRepositorySubPaths}}.
#' 
#' @inheritParams getRelativeRepositorySubPaths
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @example inst/examples/getRelativeRepositorySubPaths.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRelativeRepositorySubPaths", 
  signature = signature(
    rversion = "missing"
  ), 
  definition = function(
    rversion
  ) {
    
  getRelativeRepositorySubPaths(
    rversion = rversion
  )
    
  } 
)

