print.PackageRepository <- function(x, full = FALSE, ...) {
  if (!full) {
    print.default(x$root)
  } else {
    R6:::print.R6(x)
  }
}

#' @title
#' As URL
#' 
#' @description
#' Transform `$root` into a valid URL
#' 
#' @param scheme \code{\link{character}}.
#' @name testMethod
#'    
#' @aliases testMethod
NULL