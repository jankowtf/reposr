print.PackageRepository <- function(x, full = FALSE) {
  if (full) {
    R6:::print.R6(x)
  } else {
    print.default(x$root, full = FALSE)
  }
}

# file.exists.PackageRepository <- function(x, ...) {
#   file.exists.default(x$root)
# }
