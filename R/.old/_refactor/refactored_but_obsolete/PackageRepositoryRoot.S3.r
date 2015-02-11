#' @title 
#' Constructor for package repository class
#'
#' @description
#' Creates an instance of class \code{PackageRepositoryRoot.S3}.
#'
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{PackageRepositoryRoot.S3}. Mainly intended for rapid prototyping 
#'    purposes. As the development stage matures, the distinctive fields should
#'    be used.
#' @field path \code{\link{character}}. Installation url.
#' @field type \code{\link{character}}. Application type.
#' @return Instance of class \code{PackageRepositoryRoot.S3}.
#' @example inst/examples/PackageRepositoryRoot.S3.r
#' @template author
#' @template references
#' @keywords class, constructor
#' @export
PackageRepositoryRoot.S3 <- function(
  .x,
  path = NA_character_,
  type = c("fs", "url_fs", "url_http", "url_ftp")
) {
  cname <- "PackageRepositoryRoot.S3"
  if (!missing(.x)) {
    class(.x) <- c(cname, class(.x))
    out <- .x
  } else {
#     path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    type <- match.arg(type, c("fs", "url_fs", "url_http", "url_ftp"))
    out <- structure(
      list(path = path, type = type),
      class = c(cname, "list")
    )
  }
  return(out)
}