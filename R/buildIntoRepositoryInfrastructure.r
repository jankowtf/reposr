## TODO: possibly implement multiple binary builds
## Probably this will never actually be possible, but keep in mind ways that
## would optionally allow multiple binaries to be build (e.g. on Windows,
## Windows *and* MacOS binary).

#' Build Into Repository Infrastructure
#'
#' @description 
#' Build into repository infrastructure as ensured by 
#' \code{\link[rapp.core.repos]{ensureRepositoryInfrastructure}}.
#' 
#' @param repos_home \strong{Signature argument}.
#'    Object containing information about the repository parent location.
#' @param binary \code{\link{logical}}.
#'    In addition to the source version, also build binary version 
#'    (\code{TRUE}) or not (\code{FALSE} (default)).
#' @param remove_old \code{\link{logical}}.
#'    Remove old content (\code{TRUE}) or not (\code{FALSE} (default)).
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.repos}
#' @example inst/examples/buildIntoRepositoryInfrastructure.R
#' @export buildIntoRepositoryInfrastructure
setGeneric(name="buildIntoRepositoryInfrastructure", 
  signature = c(
    "repos_home"
  ),
  def = function(
    repos_home = file.path(Sys.getenv("HOME"), ".rapp/repos"),
    binary = FALSE,
    remove_old = FALSE,
    ...
  ) {
  standardGeneric("buildIntoRepositoryInfrastructure")
})

#' @param repos_home \code{\link{character}}. 
#' @return TODO 
#' @describeIn buildIntoRepositoryInfrastructure
#' @export
#' @import rapp.core.condition
#' @import rapp.core.description
setMethod(f = "buildIntoRepositoryInfrastructure", 
  signature = signature(
    repos_home = "character"
  ), 
  definition = function(
    repos_home,
    binary,
    remove_old,
    ...
  ) {
    
  if (!file.exists("DESCRIPTION")) {
    rapp.core.condition::signalCondition(
      condition = "InvalidUsageContext",
      msg = c(
        "Method is used in invalid context",
        Details = "expected to be called from a loaded package project"
      ),
      type = "error"
    )
  }
  
  pkg_name <- rapp.core.description::getPackageName()
  pkg_version <- rapp.core.description::getPackageVersion()
  
  ## Ensure repository infrastructure //
  ensureRepositoryInfrastructure(repos_home = repos_home)
  
  ## Get actual repositories ('global' and 'packages') //
  repos_list <- getRepositoryInfrastructurePaths(
    repos_home = repos_home,
    pkg_name = pkg_name,
    pkg_version = pkg_version
  )
  
  .buffer <- new.env()
#   ii=1
  out <- sapply(seq(along = repos_list), function(ii) {
    repos <- repos_list[[ii]]
    pkg_type <- getOption("pkgType")
    repos_path_bin <- unlist(getRepositoryPathByType(repos = repos, type = pkg_type))
    repos_path_source <- unlist(getRepositoryPathByType(repos = repos, type = "source"))
    tryCatch(
      {
        if (ii == 1) {
        ## Build and cache //
          
          devtools::document()
          
          ## Binary //
          if (binary) {
            tmp <- devtools::build(path = repos_path_bin, binary = TRUE)
            assign(pkg_type, tmp, envir = .buffer)
          }
          
          ## Source //
          tmp <- devtools::build(path = repos_path_source)
          .buffer$source <- tmp
        } else {
        ## Copy cached //
          if (!length(ls(.buffer))) {
            rapp.core.condition::signalCondition(
              condition = "EmtpyBufferObject",
              msg = c(
                "Buffer object is empty",
                Details = "expecting file paths of package build(s)"
              ),
              type = "error"
            )
          }
          
          ## Copy //
          sapply(ls(.buffer), function(ii) {
            from <- .buffer[[ii]]
            to <- switch(ii,
                   "mac.binary" = repos_path_bin,
                   "win.binary" = repos_path_bin,
                   "source" = repos_path_source
            )          
            res <- file.copy(from = from, to = to, overwrite = TRUE)
            if (!res) {
              rapp.core.condition::signalCondition(
                condition = "BuildCopyFailed",
                msg = c(
                  "Unable to copy package build",
                  From = from,
                  To = to
                ),
                type = "error"
              )  
            }
            NULL
          })
          
        }
        refreshRepositoryIndex(repos = repos) 
        if (remove_old) {
          removeOldRepositoryPackages(repos = repos, ...)
        }
        TRUE
      },
      error = function(cond) {
        message(conditionMessage(cond))
        FALSE
      },
      warning = function(cond) {
        warning(conditionMessage(cond))
        TRUE
      }
    )
  })
  names(out) <- repos_list
  out
  
  } 
)

