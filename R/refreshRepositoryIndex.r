#' Refresh Repository
#'
#' @description 
#' Refreshes repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/refreshRepositoryIndex.R
#' @export refreshRepositoryIndex
setGeneric(name="refreshRepositoryIndex", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    ...
  ) {
  standardGeneric("refreshRepositoryIndex")
})

#' @param repos \code{\link{character}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  refreshRepositoryIndex(
    repos = asRepository(repos = repos)
  )
        
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  refreshRepositoryIndex(
    repos = asExpandedRepository(repos = repos)
  )
        
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {
     
  sapply(repos, function(ii) {
    refreshRepositoryIndex(repos = ii)
  })
  TRUE
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
  
  type <- match.arg(type, c("mac.binary", "source", "win.binary"))    
  
  if (!file.exists(getRepositoryRoot(repos = repos))) {
    signalCondition(
      condition = "InvalidPackageRepositoryLocation",
      msg = c(
        "Package repository directory does not exist",
        Path = repos
      ),
      type = "error"
    )
  }    
    
  fpath <- file.path(repos, c("PACKAGES", "PACKAGES.gz"))
  wd_0 <- getwd()
  out <- tryCatch(
    {
      setwd(repos)
      
#         tools::write_PACKAGES(".", type=.Platform$pkgType)
      tools::write_PACKAGES(".", type = type)    
      TRUE
    },
    error = function(cond) {
      message(conditionMessage(cond))
      FALSE
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      TRUE
    },
    finally=setwd(wd_0)
  )
  out
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryMacBinaryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositoryType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
    
  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryWinBinaryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositoryType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
  
  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositorySourceS3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositoryType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "RappPackageRepositoryGenericS3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
  
  } 
)



