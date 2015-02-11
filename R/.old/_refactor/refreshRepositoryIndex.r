#' Refresh Repository
#'
#' @description 
#' Refreshes repository.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repor}
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
    repos = asRepositoryRoot(repos = repos)
  )
        
  } 
)

#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
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

#' @param repos \code{\link{PackageRepository.S3}}. 
#' @return \code{\link{logical}}. 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepository.S3"
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

#' @param repos \code{\link{PackageRepositorySubGeneric.S3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
#' @import conditionr
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepositorySubGeneric.S3"
  ), 
  definition = function(
    repos,
    type,
    ...
  ) {
  
  type <- match.arg(type, c("mac.binary", "source", "win.binary"))    
  
  if (!file.exists(getRepositoryRoot(repos = repos))) {
    conditionr::signalCondition(
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

#' @param repos \code{\link{PackageRepositorySubMac.S3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositorySubType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
    
  } 
)

#' @param repos \code{\link{PackageRepositorySubMac.S3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositorySubType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
  
  } 
)

#' @param repos \code{\link{PackageRepositorySubSource.S3}}. 
#' @return TODO 
#' @describeIn refreshRepositoryIndex
#' @export
setMethod(f = "refreshRepositoryIndex", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {
    
  type <- getRepositorySubType(repos = repos)
  repos <- addClassAttribute(obj = repos, 
    class_name = "PackageRepositorySubGeneric.S3")
  out <- refreshRepositoryIndex(repos = repos, type = type)
  out
  
  } 
)



