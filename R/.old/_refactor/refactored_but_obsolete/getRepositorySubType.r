#' @title 
#' Get repository sub-level type
#'
#' @description 
#' Infers the repository sub-level type based on the respective path.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getRepositorySubType", 
  signature = c(
    "repos"
  ),
  def = function(
    repos
  ) {
  standardGeneric("getRepositorySubType")
})

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos
  ) {
    
  getRepositorySubType(
    repos = asRepository(repos = repos)
  )
    
  } 
)

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
  ), 
  definition = function(
    repos
  ) {
    
  getRepositorySubType(
    repos = asRepository(repos = repos)
  )
    
  } 
)

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{PackageRepository.S3}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "PackageRepository.S3"
  ), 
  definition = function(
    repos
  ) {
    
  tmp <- sapply(repos$sublevel, function(ii) {
    basename(gsub("/contrib", "", dirname(ii)))
  })
  out <- sapply(tmp, function(ii) {
    switch(
      ii,
      "src" = "source",
      "macosx" = "mac.binary",
      "windows" = "win.binary"
    )
  })
  if (is.null(out)) {
    stop(paste0("Invalid type for path: ", tmp))
  }
  names(out) <- repos$sublevel
  out
  
  } 
)

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{PackageRepositorySubMac.S3}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "mac.binary"
  names(out) <- repos
  out
    
  } 
)

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{PackageRepositorySubWindows.S3}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "PackageRepositorySubWindows.S3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "win.binary"
  names(out) <- repos
  out  
  
  } 
)

#' @title 
#' Get repository sub-level type
#'
#' @description 
#' See generic: \code{\link[repor]{getRepositorySubType}}
#' 
#' @inheritParams getRepositorySubType
#' @param repos \code{\link{PackageRepositorySubSource.S3}}. 
#' @return TODO 
#' @example inst/examples/getRepositorySubType.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "getRepositorySubType", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    repos
  ) {
    
  out <- "source"
  names(out) <- repos
  out
  
  } 
)

