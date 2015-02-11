#' @title 
#' Normalize repository path
#'
#' @description 
#' Normalizes repository paths.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository path information.
#' @param type \strong{Signature argument}.
#'    Object containing normalization type information.
#' @example inst/examples/normalizeRepositoryPath.r
#' @template author
#' @template references
#' @keywords normalize
#' @export 
setGeneric(name="normalizeRepositoryPath", 
  signature = c(
    "repos",
    "type"
  ),
  def = function(
    repos = ".",
    type = c("fs", "url_fs", "url_http", "url_ftp")
  ) {
  standardGeneric("normalizeRepositoryPath")
})

#' @title 
#' Normalize repository path (char-char)
#'
#' @description 
#' See generic: \code{\link[repor]{normalizeRepositoryPath}}.
#' 
#' @inheritParams normalizeRepositoryPath
#' @param repos \code{\link{character}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path.
#' @example inst/examples/normalizeRepositoryPath.r
#' @template author
#' @template references
#' @keywords normalize 
#' @aliases normalizeRepositoryPath-char-char-method
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "character",
    type = "character"
  ), 
  definition = function(
    repos,
    type
  ) {
      
  type <- match.arg(type, c("fs", "url_fs", "url_http", "url_ftp"))
  
#   has_url_prefix <- length(grep(":///", repos)) > 0
  repos_raw <- gsub("///", "", gsub("^.*(?=///)", "", repos, perl = TRUE))
  repos_raw <- normalizePath(repos_raw, winslash = "/", mustWork = FALSE)
  if (type == "fs") {
    out <- repos_raw
  } else if (type == "url_fs") {
    out <- paste0("file:///", repos_raw)
  } else if (type == "url_http"){
    out <- paste0("http:///", repos_raw)
  } else if (type == "url_ftp"){
    out <- paste0("ftp:///", repos_raw)
  } else {
    out <- repos_raw
  }
  out
  
  } 
)

#' @title 
#' Normalize repository path (char-miss)
#'
#' @description 
#' See generic: \code{\link[repor]{normalizeRepositoryPath}}.
#' 
#' @inheritParams normalizeRepositoryPath
#' @param repos \code{\link{character}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @example inst/examples/normalizeRepositoryPath.r
#' @template author
#' @template references
#' @keywords normalize
#' @aliases normalizeRepositoryPath-char-miss-method
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "character",
    type = "missing"
  ), 
  definition = function(
    repos,
    type
  ) {
    
  normalizeRepositoryPath(
    repos = repos,
    type = type
  )
    
  } 
)

#' @title 
#' Normalize repository path (PackageRepositoryRoot.S3-char)
#'
#' @description 
#' See generic: \code{\link[repor]{normalizeRepositoryPath}}.
#' 
#' @inheritParams normalizeRepositoryPath
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @example inst/examples/normalizeRepositoryPath.r
#' @template author
#' @template references
#' @keywords normalize
#' @aliases normalizeRepositoryPath-PackageRepositoryRoot.S3-char-method
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3",
    type = "character"
  ), 
  definition = function(
    repos,
    type
  ) {
    
  repos$path <- normalizeRepositoryPath(
    repos = repos$path,
    type = type
  )
  repos
    
  } 
)

#' @title 
#' Normalize repository path (PackageRepositoryRoot.S3-miss)
#'
#' @description 
#' See generic: \code{\link[repor]{normalizeRepositoryPath}}.
#' 
#' @inheritParams normalizeRepositoryPath
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @example inst/examples/normalizeRepositoryPath.r
#' @template author
#' @template references
#' @keywords normalize
#' @aliases normalizeRepositoryPath-PackageRepositoryRoot.S3-miss-method
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3",
    type = "missing"
  ), 
  definition = function(
    repos,
    type
  ) {
    
  repos$path <- normalizeRepositoryPath(
    repos = repos$path,
    type = type
  )
  repos
    
  } 
)
