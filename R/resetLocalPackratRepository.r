#' Reset Local Packrat Repository
#'
#' @description 
#' Reset local packrat package repository.
#' 
#' @param opt_name \strong{Signature argument}.
#'    Object containing option name information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/repositr}
#' @example inst/examples/resetLocalPackratRepository.R
#' @export resetLocalPackratRepository
setGeneric(name="resetLocalPackratRepository", 
  signature = c(
    "opt_name"
  ),
  def = function(
    opt_name = "local.repos"
  ) {
    standardGeneric("resetLocalPackratRepository")
  })

#' @param opt_name \code{\link{character}}. 
#' @return \code{logical}. \code{TRUE}. 
#' @describeIn resetLocalPackratRepository
#' @export
setMethod(f = "resetLocalPackratRepository", 
  signature = signature(
    opt_name = "character"
  ), 
  definition = function(
    opt_name
  ) {
    
  packrat::set_opts(local.repos = character())
  TRUE
  
  } 
)

#' @param opt_name \code{\link{missing}}. 
#' @return see \code{character} method. 
#' @describeIn resetLocalPackratRepository
#' @export
setMethod(f = "resetLocalPackratRepository", 
  signature = signature(
    opt_name = "missing"
  ), 
  definition = function(
    opt_name
  ) {
    
  resetLocalPackratRepository(opt_name = opt_name)
    
  } 
)

