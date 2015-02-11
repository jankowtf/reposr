#' @title 
#' Class: PackageRepositorySubMac.S3
#'
#' @description
#' Class constructor. 
#' Creates an instance of class \code{PackageRepositorySubMac.S3}.
#'
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{PackageRepositorySubMac.S3}. Mainly intended for rapid prototyping 
#'    purposes. As the development stage matures, the distinctive fields should
#'    be used.
#' @field path \code{\link{character}}. Installation url.
#' @return Instance of class \code{PackageRepositorySubMac.S3}.
#' @example inst/examples/PackageRepositorySubMac.S3.r
#' @template author
#' @template references
#' @keywords class, constructor
#' @export
PackageRepositorySubMac.S3 <- function(
  .x,
  path = NA_character_
) {
  cname <- "PackageRepositorySubMac.S3"
  if (!missing(.x)) {
    class(.x) <- c(cname, class(.x))
    out <- .x
  } else {
    out <- structure(
      list(path = path),
      class = c(cname, "list")
    )
  }
  return(out)
}