#' @title
#' Ensure repository
#'
#' @description 
#' Ensures existence of a valid R package repository based on suitable input
#' information (which is always reduced to the repository's root path).
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param rversion \strong{Signature argument}.
#'    Object containing version information.
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @export
setGeneric(
  name = "ensureRepository", 
  signature = c(
    "repos",
    "rversion"
  ),
  def = function(
    repos = ".",
    rversion = paste(R.version$major, 
      unlist(strsplit(R.version$minor, split = "\\."))[2], sep = ".")
  ) {
  standardGeneric("ensureRepository")
  }
)

#' @title
#' Ensure repository (char-miss)
#'
#' @description 
#' See generic: \code{\link{ensureRepository}}.
#' 
#' @inheritParams ensureRepository
#' @param repos \code{\link{character}}. 
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @aliases ensureRepository-char-miss-method
#' @export
setMethod(
  f = "ensureRepository", 
  signature = signature(
    repos = "character",
    rversion = "missing"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = asRepositoryRoot(repos, type = "fs"),
    rversion = rversion
  )
    
  } 
)

#' @title
#' Ensure repository (char-char)
#'
#' @description 
#' See generic: \code{\link{ensureRepository}}.
#' 
#' @inheritParams ensureRepository
#' @param repos \code{\link{character}}. 
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @aliases ensureRepository-char-char-method
#' @export
setMethod(
  f = "ensureRepository", 
  signature = signature(
    repos = "character",
    rversion = "character"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = asRepositoryRoot(repos, type = "fs"),
    rversion = rversion
  )
    
  } 
)

#' @title
#' Ensure repository (PackageRepositoryRoot.S3-miss)
#'
#' @description 
#' See generic: \code{\link{ensureRepository}}.
#' 
#' @inheritParams ensureRepository
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @param rversion \code{\link{missing}}. 
#' @return TODO 
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @aliases ensureRepository-PackageRepositoryRoot.S3-miss-method
#' @export
setMethod(
  f = "ensureRepository", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3",
    rversion = "missing"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  ensureRepository(
    repos = repos,
    rversion = rversion
  )
    
  } 
)

#' @title
#' Ensure repository (PackageRepositoryRoot.S3-char)
#'
#' @description 
#' See generic: \code{\link{ensureRepository}}.
#' 
#' @inheritParams ensureRepository
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @param rversion \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @aliases ensureRepository-PackageRepositoryRoot.S3-char-method
#' @export
setMethod(
  f = "ensureRepository", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3",
    rversion = "character"
  ), 
  definition = function(
    repos,
    rversion
  ) {

  out <- ensureRepository(
    repos = asRepository(repos = repos, rversion = rversion)
  )
  names(out) <- repos$path
  out
  
  } 
)

#' @title
#' Ensure repository (PackageRepository.S3-any)
#'
#' @description 
#' See generic: \code{\link{ensureRepository}}.
#' 
#' @inheritParams ensureRepository
#' @param repos \code{\link{PackageRepository.S3}}. 
#' @param rversion \code{\link{ANY}}. 
#' @return TODO 
#' @example inst/examples/ensureRepository.r
#' @template author
#' @template references
#' @aliases ensureRepository-PackageRepository.S3-any-method
#' @export
setMethod(f = "ensureRepository", 
  signature = signature(
    repos = "PackageRepository.S3",
    rversion = "ANY"
  ), 
  definition = function(
    repos,
    rversion
  ) {
    
  sapply(repos$sublevel, dir.create, recursive = TRUE, showWarnings = FALSE)
  ensureRepositoryIndexFiles(repos = repos)
  out <- TRUE
  names(out) <- repos$root$path
  out
  
  } 
)

