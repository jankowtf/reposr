#' Normalize Repository Path
#'
#' @description 
#' Normalizes repository paths.
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @param type \strong{Signature argument}.
#'    Object containing normalization type information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/normalizeRepositoryPath.R
#' @export normalizeRepositoryPath
setGeneric(name="normalizeRepositoryPath", 
  signature = c(
    "path",
    "type"
  ),
  def = function(
      path=".",
      type=c("filesystem", "url")
  ) {
  standardGeneric("normalizeRepositoryPath")
})

#' @param path \code{\link{character}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
      path = "character",
      type = "character"
  ), 
  definition = function(
      path,
      type
  ) {
      
  type <- match.arg(type, c("filesystem", "url"))
  if (type == "filesystem") {
    out <- gsub("^file:///", "", path)
  } else if (type == "url") {
    if (!grepl("^file:///", path)) {
      out <- paste0("file:///", path)
    } else {
      out <- path
    }
  }
  out
  
  } 
)

#' @param path \code{\link{character}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    path = "character",
    type = "missing"
  ), 
  definition = function(
    path,
    type
  ) {
    
    normalizeRepositoryPath(
      path=path,
      type=type
    )
    
  } 
)

#' @param path \code{\link{RappRepositoryS3}}. 
#' @param type \code{\link{character}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    path = "RappRepositoryS3",
    type = "character"
  ), 
  definition = function(
    path,
    type
  ) {
    
    normalizeRepositoryPath(
      path = as.character(path),
      type = type
    )
    
  } 
)

#' @param path \code{\link{RappRepositoryS3}}. 
#' @param type \code{\link{missing}}. 
#' @return \code{\link{character}}. Normalized path. 
#' @describeIn normalizeRepositoryPath
#' @export
setMethod(f = "normalizeRepositoryPath", 
  signature = signature(
    path = "RappRepositoryS3",
    type = "missing"
  ), 
  definition = function(
    path,
    type
  ) {
    
    normalizeRepositoryPath(
      path = as.character(path),
      type = type
    )
    
  } 
)
