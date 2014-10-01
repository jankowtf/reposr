#' Ensure Repository Registry Files
#'
#' @description 
#' Ensures existence of a repository's registry files (\code{PACKAGES} and
#' \code{PACKAGES.gz}.
#' 
#' @param repos \strong{Signature argument}.
#'    Object containing repository information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/ensureRepositoryIndex.R
#' @seealso \code{\link[rapp2]{ensureRepositoryIndex-character-method}}
#' @export ensureRepositoryIndex
setGeneric(name="ensureRepositoryIndex", 
  signature = c(
    "repos"
  ),
  def = function(
    repos,
    ...
  ) {
  standardGeneric("ensureRepositoryIndex")
})

#' @param repos \code{\link{character}}. 
#' @return TODO 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
  signature = signature(
    repos = "character"
  ), 
  definition = function(
    repos,
    ...
  ) {

  return(ensureRepositoryIndex(
      repos = asExpandedRepository(repos = repos, ...)
  ))
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryS3}}. 
#' @return TODO 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  return(ensureRepositoryIndex(
      repos = asExpandedRepository(repos = repos, ...)
  ))
  
  } 
)

#' @param repos \code{\link{RappExpandedPackageRepositoryS3}}. 
#' @return TODO 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
  signature = signature(
    repos = "RappExpandedPackageRepositoryS3"
  ), 
  definition = function(
    repos,
    ...
  ) {

  out <- sapply(repos, function(ii) {
    ensureRepositoryIndex(repos = ii, ...)
  })
  names(out) <- repos
  out
  
  } 
)

#' @param repos \code{\link{RappPackageRepositoryGenericS3}}. 
#' @describeIn ensureRepositoryIndex
#' @export
#' @import conditionr
setMethod(f = "ensureRepositoryIndex", 
  signature = signature(
    repos = "RappPackageRepositoryGenericS3"
  ), 
  definition = function(
    repos,
    type = c("mac.binary", "source", "win.binary"),
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
  if (!all(file.exists(fpath))) {
    wd_0   <- getwd()
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
  } else {
    out <- TRUE
  }
  names(out) <- repos
  out

  } 
)

#' @param repos \code{\link{RappPackageRepositoryMacBinaryS3}}. 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
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
  out <- ensureRepositoryIndex(repos = repos, type = type)
  out

  } 
)

#' @param repos \code{\link{RappPackageRepositorySourceS3}}. 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
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
  out <- ensureRepositoryIndex(repos = repos, type = type)
  out

  } 
)

#' @param repos \code{\link{RappPackageRepositoryWinBinaryS3}}. 
#' @describeIn ensureRepositoryIndex
#' @export
setMethod(f = "ensureRepositoryIndex", 
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
  out <- ensureRepositoryIndex(repos = repos, type = type)
  out

  } 
)



