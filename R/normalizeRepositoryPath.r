#' Normalize Repository Path
#'
#' @description 
#' Normalizes repository paths.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository path information.
#' @param type \strong{Signature argument}.
#'    Object containing normalization type information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/normalizeRepositoryPath.R
#' @export normalizeRepositoryPath
setGeneric(name="normalizeRepositoryPath", 
  signature = c(
    "repos",
    "type"
  ),
  def = function(
    repos = ".",
    type = c("fs", "url_file", "url_http", "url_ftp")
  ) {
  standardGeneric("normalizeRepositoryPath")
})

#' @param repos \code{\link{character}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
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
      
  type <- match.arg(type, c("fs", "url_file", "url_http", "url_ftp"))
  
#   has_url_prefix <- length(grep(":///", repos)) > 0
  repos_raw <- gsub("///", "", gsub("^.*(?=///)", "", repos, perl = TRUE))
  repos_raw <- normalizePath(repos_raw, winslash = "/", mustWork = FALSE)
  if (type == "fs") {
    out <- repos_raw
  } else if (type == "url_file") {
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

#' @param repos \code{\link{character}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
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

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "RappPackageRepositoryS3",
    type = "character"
  ), 
  definition = function(
    repos,
    type
  ) {
    
  normalizeRepositoryPath(
    repos = as.character(repos),
    type = type
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    repos = "RappPackageRepositoryS3",
    type = "missing"
  ), 
  definition = function(
    repos,
    type
  ) {
    
  normalizeRepositoryPath(
    repos = as.character(repos),
    type = type
  )
    
  } 
)
