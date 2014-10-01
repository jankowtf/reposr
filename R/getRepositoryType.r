#' Get Repository Type
#'
#' @description 
#' Infers the repository type based on repository location.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/getRepositoryType.R
#' @export getRepositoryType
setGeneric(name="getRepositoryType", 
  signature = c(
    "repos"
  ),
  def = function(
      repos
  ) {
  standardGeneric("getRepositoryType")
})

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
      repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
      repos
  ) {
    
  tmp <- sapply(repos, function(ii) {
    basename(gsub("/contrib", "", dirname(ii)))
  })
  out <- sapply(tmp, function(ii) {
    switch(
      ii,
      "src"="source",
      "macosx"="mac.binary",
      "windows"="win.binary"
    )
  })
  if (is.null(out)) {
    stop(paste0("Invalid type for path: ", tmp))
  }
  names(out) <- repos
  out
  
  } 
)

#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos
  ) {
    
  getRepositoryType(
    repos = asExpandedRepository(repos = repos)
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos
  ) {
    
  getRepositoryType(
    repos = asExpandedRepository(repos = repos)
  )
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "mac.binary"
  names(out) <- repos
  out
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "win.binary"
  names(out) <- repos
  out  
  
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @return TODO 
#' @describeIn getRepositoryType
#' @export
setMethod(f = "getRepositoryType", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "source"
  names(out) <- repos
  out
  
  } 
)

