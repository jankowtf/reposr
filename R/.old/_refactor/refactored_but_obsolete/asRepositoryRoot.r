#' @title 
#' As Repository
#'
#' @description 
#' Turns suitable input objects into instances of class 
#' \code{\link[repor]{PackageRepositoryRoot.S3}}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @param type \code{\link{character}}.
#'    Location type. One of \code{ c("fs", "url_fs", "url_http", "url_ftp")}.
#'    See \code{\link[repor]{normalizeRepositoryPath}}.
#' @param ensure \code{logical}.
#'    \code{TRUE}: ensure repository existence;
#'    \code{FALSE}: existence is not ensured.
#' @param ... Further arguments passed to subsequent functions.
#'    In particular:
#'    \itemize{
#'      \item{\code{\link[repor]{ensureRepository}}}.
#'    }
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @seealso \code{
#'    \link[repor]{normalizeRepositoryPath}
#' }
#' @export
setGeneric(
  name = "asRepositoryRoot", 
  signature = c(
    "repos"
  ),
  def = function(
    repos = ".",
    type =  c("fs", "url_fs", "url_http", "url_ftp"),
    ensure = FALSE,
    ...
  ) {
    standardGeneric("asRepositoryRoot")
  })

#' @title 
#' As Repository (miss)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{missing}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-miss-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "missing"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
  asRepositoryRoot(
    repos = repos,
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @title 
#' As Repository (char)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{character}}.  
#' @return \code{PackageRepositoryRoot.S3}. Identical to \code{repos} with updated
#'   	class table. 
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-char-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    type, 
    ensure,
    ...
  ) {
    
  repos <- normalizeRepositoryPath(repos = repos, type = type)     
  
  ## Dispatch to `PackageRepositoryRoot.S3` method //
  asRepositoryRoot(
    repos = PackageRepositoryRoot.S3(path = repos, type = type), 
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

#' @title 
#' As Repository (PackageRepositoryRoot.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepositoryRoot.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-PackageReppository.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
  if (ensure) {
    ensureRepository(repos = repos, ...)
    ## ...: 'rversion'
  }    
  normalizeRepositoryPath(repos = repos, type = type, ...)
    
  } 
)

#' @title 
#' As Repository (PackageRepository.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepository.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-ExpandedPackageReppository.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepository.S3"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
    ## Keep for reference //
    #   patterns <- getRelativeRepositorySubPaths(rversion = rversion)
    #   repos <- unique(sapply(names(repos), function(ii) {
    #     gsub(paste0("/", patterns[[ii]]), "", repos[[ii]])
    #   }))
    
    ## Dispatch to 'PackageRepositoryRoot.S3' //
    asRepositoryRoot(
      repos = getRepositoryRoot(repos = repos),
      ensure = ensure,
      type = type,
      ...
    )
    
  } 
)

#' @title 
#' As Repository (PackageRepositorySubGeneric.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubGeneric.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-PackageReppositoryGeneric.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubGeneric.S3"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
    ## Dispatch to 'PackageRepositoryRoot.S3' //
    asRepositoryRoot(
      repos = getRepositoryRoot(repos = repos),
      ensure = ensure,
      type = type,
      ...
    )
    
  } 
)

#' @title 
#' As Repository (PackageRepositorySubMac.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubMac.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-PackageReppositoryMacBinary.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
    ## Dispatch to 'PackageRepositoryRoot.S3' //
    asRepositoryRoot(
      repos = getRepositoryRoot(repos = repos),
      ensure = ensure,
      type = type,
      ...
    )
    
  } 
)

#' @title 
#' As Repository (PackageRepositorySubMac.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubMac.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-PackageReppositoryWinBinary.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos,
    type,
    ensure,
    ...
  ) {
    
    ## Dispatch to 'PackageRepositoryRoot.S3' //
    asRepositoryRoot(
      repos = getRepositoryRoot(repos = repos),
      ensure = ensure,
      type = type,
      ...
    )
    
  } 
)

#' @title 
#' As Repository (PackageRepositorySubSource.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{asRepositoryRoot}}.
#' 
#' @inheritParams asRepositoryRoot
#' @param repos \code{\link{PackageRepositorySubSource.S3}}.  
#' @example inst/examples/asRepositoryRoot.r
#' @template author
#' @template references
#' @aliases asRepositoryRoot-PackageReppositorySource.S3-method
#' @export
setMethod(
  f = "asRepositoryRoot", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    repos,
    type, 
    ensure,
    ...
  ) {
    
  ## Dispatch to 'PackageRepositoryRoot.S3' //
  asRepositoryRoot(
    repos = getRepositoryRoot(repos = repos),
    ensure = ensure,
    type = type,
    ...
  )
    
  } 
)

