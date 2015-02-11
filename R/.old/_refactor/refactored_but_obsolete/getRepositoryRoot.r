#' @title 
#' Get Repository root path
#'
#' @description 
#' Retrieves a repository's root path.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @export
setGeneric(
  name = "getRepositoryRoot", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = "."
  ) {
  standardGeneric("getRepositoryRoot")
})

#' @title 
#' Get Repository root path (miss)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{missing}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-miss-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos
  ) {
    
  getRepositoryRoot(repos = repos)
    
  } 
)

#' @title 
#' Get Repository root path (char)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{character}}.  
#' @return \code{PackageRepositoryRoot.S3}. No matter what type repository has 
#'    been passed in, the method returns only the root part of the repository.
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-char-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos
  ) {
      
  asRepositoryRoot(repos = repos)
  
  } 
)

#' @title 
#' Get Repository root path (PackageRepositoryRoot.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{PackageRepositoryRoot.S3}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-PackageRepositoryRoot.S3-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
  ), 
  definition = function(
    repos
  ) {
  
  repos
    
  } 
)

#' @title 
#' Get Repository root path (PackageRepository.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{PackageRepository.S3}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-PackageRepository.S3-method
#' @export
#' @import conditionr
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "PackageRepository.S3"
  ), 
  definition = function(
    repos
  ) {
  
  tmp <- unique(sapply(lapply(repos$sublevel, getRepositoryRoot), "[[", "path"))
  if (length(tmp) > 1) {
    conditionr::signalCondition(
      condition = "Ambiguous package repository structure",
      msg = c(
        "Unable to identify unambiguous root path",
        Found = paste(tmp, collapse = ", ")
      ),
      type = "error"
    )
  }
  asRepositoryRoot(repos = tmp)
  
  } 
)

#' @title 
#' Get Repository root path (PackageRepositorySubMac.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubMac.S3}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-PackageRepositorySubMac.S3-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos
  ) {
  
#   repos <- PackageRepositorySubGeneric.S3(path = repos$path)    
#   getRepositoryRoot(repos = repos)
  sublevels <- getRelativeRepositorySubPaths()  
  out <- asRepositoryRoot(
    repos = gsub(paste0("/", sublevels["mac.binary"], "$"), "", unclass(repos))
  )
  
  } 
)

#' @title 
#' Get Repository root path (PackageRepositorySubWindows.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubWindows.S3}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-PackageRepositorySubWindows.S3-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubWindows.S3"
  ), 
  definition = function(
    repos
  ) {
  
#   repos <- PackageRepositorySubGeneric.S3(path = repos$path)    
#   getRepositoryRoot(repos = repos)
  sublevels <- getRelativeRepositorySubPaths()  
  out <- asRepositoryRoot(
    repos = gsub(paste0("/", sublevels["win.binary"], "$"), "", unclass(repos))
  )
  
  } 
)

#' @title 
#' Get Repository root path (PackageRepositorySubSource.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositoryRoot}}.
#' 
#' @inheritParams getRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubSource.S3}}.  
#' @example inst/examples/getRepositoryRoot.r
#' @template author
#' @template references
#' @aliases getRepositoryRoot-PackageRepositorySubSource.S3-method
#' @export
setMethod(
  f = "getRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    repos
  ) {
  
#   repos <- PackageRepositorySubGeneric.S3(path = repos$path)    
#   getRepositoryRoot(repos = repos)
  sublevels <- getRelativeRepositorySubPaths()  
  out <- asRepositoryRoot(
    repos = gsub(paste0("/", sublevels["source"], "$"), "", unclass(repos))
  )
  
  } 
)

# #' @title 
# #' Get Repository root path
# #'
# #' @description 
# #' See generic: \code{\link[repor]{getRepositoryRoot}}.
# #' 
# #' @inheritParams getRepositoryRoot
# #' @param repos \code{\link{PackageRepositorySubGeneric.S3}}.  
# #' @example inst/examples/getRepositoryRoot.r
# #' @template author
# #' @template references
# #' @export
# setMethod(
#   f = "getRepositoryRoot", 
#   signature = signature(
#     repos = "PackageRepositorySubGeneric.S3"
#   ), 
#   definition = function(
#     repos
#   ) {
#     
#   out <- asRepositoryRoot(
#     repos = unclass(gsub("(?<=repos)/.*", "", repos, perl = TRUE))
#   )
#   return(out)
#     
#   } 
# )