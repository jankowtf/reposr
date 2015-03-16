#' @export
print.PackageRepository <- function(x, full = FALSE, ...) {
  if (!full) {
    print.default(x$root)
  } else {
    R6:::print.R6(x)
  }
}
getPrivate <- function(x) {
  environment(x$ensure)$private
}