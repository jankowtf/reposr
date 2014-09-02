#' Get Package Versions based on Repository Index File
#'
#' @description 
#' Get package versions based on a repository's index file (\code{PACKAGES}).
#' 
#' @param input \strong{Signature argument}.
#' 		Object containing source input for parsing.
#' @param ... Further arguments passed to:
#' 		\code{\link[rapp.core.repos]{asExpandedRepository}},
#' 		\code{\link[rapp.core.repos]{hasRepositoryPackages}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
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
#   x <- c("rapp.core.repos_0.1.0.1.zip", "rapp.core.b_0.1.0.1.zip", 
#          "rapp.core.repos_0.1.0.2.zip", "rapp.core.b_0.1.0.1.1.zip")
#   grep(out[2], x)
  out

  }
)

#' @param input  \code{\link{RappPackageRepositoryS3}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappPackageRepositoryS3"
  ), 
  definition=function(
    input,
    ...
  ) {
    
  return(getPackageBuildFilePatterns(
    input <- asExpandedRepository(repos = input, ...)
  ))
  
  }
)

#' @param input  \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappExpandedPackageRepositoryS3"
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

#' @param input \code{\link{RappPackageRepositoryGenericS3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    input,
    ...
  ) {
  
  
  if (!file.exists(getRepositoryRoot(repos = input))) {
    signalCondition(
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

#' @param input \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    input,
    ...
  ) {
  
  input <- addClassAttribute(obj = input, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
    
  } 
)

#' @param input \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    input,
    type,
    ...
  ) {
    
  input <- addClassAttribute(obj = input, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
  
  } 
)

#' @param input \code{\link{RappPackageRepositorySourceS3}}. 
#' @return TODO 
#' @describeIn getPackageBuildFilePatterns
#' @export
setMethod(f = "getPackageBuildFilePatterns", 
  signature = signature(
    input = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    input,
    type,
    ...
  ) {
    
  input <- addClassAttribute(obj = input, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- getPackageBuildFilePatterns(
    input = input,
    ...
  )
  out
  
  } 
)

