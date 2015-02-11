#' @title 
#' Ensure repository index
#'
#' @description 
#' Ensures the existence of a repository's index/registry files 
#' (\code{PACKAGES} and \code{PACKAGES.gz}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @seealso \code{\link[rapp2]{ensureRepositoryIndexFiles-character-method}}
#' @export
setGeneric(
  name = "ensureRepositoryIndexFiles", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    ...
  ) {
  standardGeneric("ensureRepositoryIndexFiles")
})

#' @title 
#' Ensure repository index (char)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ...
  ) {

  ensureRepositoryIndexFiles(
    repos = asRepository(repos = repos, ...)
  )
  
  } 
)

#' @title 
#' Ensure repository index (PackageRepositoryRoot.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepositoryRoot.S3}}. 
#' @return TODO 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepositoryRoot.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  ensureRepositoryIndexFiles(
    repos = asRepository(repos = repos, ...)
  )
  
  } 
)

#' @title 
#' Ensure repository index (PackageRepository.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepository.S3}}. 
#' @return TODO 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepository.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  out <- sapply(repos$sublevel, function(ii) {
    ensureRepositoryIndexFiles(repos = ii, ...)
  })
  names(out) <- repos
  out
  
  } 
)

#' @title 
#' Ensure repository index (PackageRepositorySubMac.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepositorySubMac.S3}}. 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepositorySubMac.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  type <- getRepositorySubType(repos = repos)
  repos <- PackageRepositorySubGeneric.S3(.x = repos)
  out <- ensureRepositoryIndexFiles(repos = repos, type = type)
  out

  } 
)

#' @title 
#' Ensure repository index (PackageRepositorySubSource.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepositorySubSource.S3}}. 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepositorySubSource.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  type <- getRepositorySubType(repos = repos)
  repos <- PackageRepositorySubGeneric.S3(.x = repos)
  out <- ensureRepositoryIndexFiles(repos = repos, type = type)
  out

  } 
)

#' @title 
#' Ensure repository index (PackageRepositorySubWindows.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepositorySubWindows.S3}}. 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepositorySubWindows.S3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  type <- getRepositorySubType(repos = repos)
  repos <- PackageRepositorySubGeneric.S3(.x = repos)
  out <- ensureRepositoryIndexFiles(repos = repos, type = type)
  out

  } 
)

#' @title 
#' Ensure repository index (PackageRepositorySubGeneric.S3)
#'
#' @description 
#' See generic: \code{\link[repor]{ensureRepositoryIndexFiles}}
#' 
#' @inheritParams ensureRepositoryIndexFiles
#' @param repos \code{\link{PackageRepositorySubGeneric.S3}}. 
#' @example inst/examples/ensureRepositoryIndexFiles.r
#' @template author
#' @template references
#' @export
#' @import conditionr
setMethod(
  f = "ensureRepositoryIndexFiles", 
  signature = signature(
    repos = "PackageRepositorySubGeneric.S3"
  ), 
  definition = function(
    repos,
    type = c("mac.binary", "source", "win.binary"),
    ...
  ) {
    
  type <- match.arg(type, c("mac.binary", "source", "win.binary"))    
  
  if (!file.exists(getRepositoryRoot(repos = repos)$path)) {
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
  out <- if (!all(file.exists(fpath))) {
    wd_0   <- getwd()
    tryCatch({
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
  } else {
    TRUE
  }
  names(out) <- repos
  out

  } 
)


