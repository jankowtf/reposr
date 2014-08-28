"[.RappExpandedPackageRepositoryS3" <- function(x, i, ...) {
  out <- I(NextMethod("["))
  class(out) <- class(x)
  out
}