#' Parse Repository Index File
#'
#' @description 
#' Parses a repository's index file (\code{PACKAGES}).
#' 
#' @param path \strong{Signature argument}.
#' 		Object containing source information for parsing.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/parseRepositoryIndexFile.R
#' @export 
setGeneric(name = "parseRepositoryIndexFile", 
  signature = c(
    "path"
  ),
  def=function(
    path = "PACKAGES"
  ) {
  standardGeneric("parseRepositoryIndexFile")
})

#' @param path  \code{\link{missing}}. 
#' @describeIn parseRepositoryIndexFile
#' @export
setMethod(f="parseRepositoryIndexFile", 
  signature=signature(
    path = "missing"
  ), 
  definition=function(
    path
  ) {

  parseRepositoryIndexFile(
    path = path
  )
      
  }
)

#' @param path  \code{\link{character}}. 
#' @describeIn parseRepositoryIndexFile
#' @export
setMethod(f = "parseRepositoryIndexFile", 
  signature = signature(
    path = "character"
  ), 
  definition=function(
    path
  ) {
      
  if (!file.exists(path)) {
    signalCondition(
      condition = "InvalidPathToPackagesIndexFile",
      msg = c(
        "Unable to find index file 'PACKAGES'",
        Path = path
      ),
      type = "error"
    )
  }
  
#   out <- list()
#   if (length(fields)) {
#       fields <- as.character(fields)
#       out[fields] <- NA
#   }
  
#   cnt <- readLines(path)
  dcf <- as.data.frame(read.dcf(path), stringsAsFactors = FALSE)
  dcf <- addClassAttribute(obj = dcf, 
    class_name = "RappParsedPackageRepositoryIndexS3")
  dcf
#   if (length(fields)) {
#     ok <- names(dcf) %in% fields
#     dcf[names(dcf)[ok]] <- dcf[ok]
#   } else {
#     dcf[names(dcf)] <- dcf
#   }

  }
)

