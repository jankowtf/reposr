#' Add Class Attribute
#'
#' @description 
#' Adds class attribute to an object and returns it.
#' 
#' @param obj \strong{Signature argument}.
#'    Object to be updated.
#' @param class_name \strong{Signature argument}.
#'    Object containing class name information.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/rapp.core.obj}
#' @example inst/examples/addClassAttribute.R
#' @export addClassAttribute
setGeneric(name="addClassAttribute", 
  signature = c(
    "obj",
    "class_name"
  ),
  def = function(
    obj,
    class_name
  ) {
  standardGeneric("addClassAttribute")
})

#' @param obj \code{\link{ANY}}.  
#' @param class_name \code{\link{character}}.  
#' @return Object with updated class graph (\code{c(<class_name>, <class>)}).
#' @describeIn addClassAttribute
#' @export
setMethod(f = "addClassAttribute", 
  signature = signature(
    obj = "ANY",
    class_name = "character"
  ), 
  definition = function(
    obj,
    class_name
  ) {
      
  class_name <- match.arg(
    class_name,
    choices = c(
      "PackageRepository.S3",
      "PackageRepositoryRoot.S3",
      "PackageRepositorySubGeneric.S3",
      "PackageRepositorySubMac.S3",
      "PackageRepositorySubSource.S3",
      "PackageRepositorySubMac.S3",
      "RappPackageRepositoryListS3",
      "RappParsedPackageRepositoryIndexS3"
    )
  )
  class(obj) <- unique(c(class_name, class(obj)))
  obj
    
  } 
)

