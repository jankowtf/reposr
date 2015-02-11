#' Get Package Versions based on Repository Index File
#'
#' @description 
#' Get package versions based on a repository's index file (\code{PACKAGES}).
#' 
#' @param input \strong{Signature argument}.
#' 		Object containing source input for parsing.
#' @param ... Further arguments passed to:
#' 		\code{\link[repor]{asRepository}},
#' 		\code{\link[repor]{hasRepositoryPackages}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
#' @example inst/examples/getPackageBuildFilePatterns.R
#' @export
setGeneric(name = "getPackageBuildFilePatterns", 
  signature = c(
    "input"
  ),
  def=function(
    input = "PACKAGES",
    ...
  ) {
  standardGeneric("getPackageBuildFilePatterns")
})

#' @param input  \code{\link{missing}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f="getPackageBuildFilePatterns", 
  signature=signature(
    input = "missing"
  ), 
  definition=function(
    input,
    ...
  ) {

  getPackageBuildFilePatterns(
    input = input,
    ...
  )
      
  }
)

#' @param input  \code{\link{character}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "character"
  ), 
  definition=function(
    input,
    ...
  ) {
      
  getPackageBuildFilePatterns(    
    input = parseRepositoryIndexFile(path = input),
    ...
  )
  
  }
)

#' @param input  \code{\link{RappParsedPackageRepositoryIndexS3}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappParsedPackageRepositoryIndexS3"
  ), 
  definition=function(
    input,
    ...
  ) {
      
  raw_vec <- paste0(input[, "Package"], "_", input[, "Version"])

#   out <- lapply(c(".tar", ".tar.gz", ".zip"), function(ii) {
#     paste0(raw_vec, ii)
#   })
#   names(out) <- c("mac.binary", "source", "win.binary")
#   out   
  out <- paste0(raw_vec, "\\.\\w.*$")
#   x <- c("repositr_0.1.0.1.zip", "rapp.core.b_0.1.0.1.zip", 
#          "repositr_0.1.0.2.zip", "rapp.core.b_0.1.0.1.1.zip")
#   grep(out[2], x)
  out

  }
)

#' @param input  \code{\link{PackageRepositoryRoot.S3}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepositoryRoot.S3"
  ), 
  definition=function(
    input,
    ...
  ) {
    
  return(getPackageBuildFilePatterns(
    input <- asRepository(repos = input, ...)
  ))
  
  }
)

#' @param input  \code{\link{PackageRepository.S3}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepository.S3"
  ), 
  definition=function(
    input,
    ...
  ) {
    
  ## Existence check //
  exist_idx <- hasRepositoryPackages(repos = input, ...)
  out <- as.list(rep(NA, length(exist_idx)))
  names(out) <- names(exist_idx)
  if (all(!exist_idx)) {
    
  } else {
    tmp <- lapply(input[exist_idx], function(ii) {
      unlist(unname(getPackageBuildFilePatterns(input = ii, ...)))
    })
    names(tmp) <- names(exist_idx[exist_idx])
    out[names(tmp)] <- tmp
  }
  out
  
  }
)

#' @param input \code{\link{PackageRepositorySubGeneric.S3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
#' @import conditionr
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepositorySubGeneric.S3"
  ), 
  definition = function(
    input,
    ...
  ) {
  
  
  if (!file.exists(getRepositoryRoot(repos = input))) {
    conditionr::signalCondition(
      condition = "InvalidPackageRepositoryLocation",
      msg = c(
        "Package repository directory does not exist",
        Path = input
      ),
      type = "error"
    )
  }    
  
  out <- getPackageBuildFilePatterns(    
    input = parseRepositoryIndexFile(path = file.path(input, "PACKAGES")),
        ...
  )
  if (all(out == "_\\.\\w.*$")) {
    out <- NA
  }
  out <- list(out)
  names(out) <- input
  out
    
  } 
)

#' @param input \code{\link{PackageRepositorySubMac.S3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    input,
    ...
  ) {
  
  input <- addClassAttribute(obj = input, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
    
  } 
)

#' @param input \code{\link{PackageRepositorySubMac.S3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    input,
    type,
    ...
  ) {
    
  input <- addClassAttribute(obj = input, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
  
  } 
)

#' @param input \code{\link{PackageRepositorySubSource.S3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    input,
    type,
    ...
  ) {
    
  input <- addClassAttribute(obj = input, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
  
  } 
)

