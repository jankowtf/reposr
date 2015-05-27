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
getRversion <- function(type = c("minor", "major", "patch")) {
  type <- match.arg(type, c("minor", "major", "patch"))
  switch(type,
    "major" = version$major,
    "minor" = sprintf("%s.%s", version$major, 
      unlist(strsplit(version$minor, "\\."))[1]),
    "patch" = sprintf("%s.%s", version$major, version$minor)
  )
}
# getRversion()
# getRversion("major")
# getRversion("patch")

adaptRversionNumber <- function(path) {
  rversion <- getRversion("minor")
  dirs <- list.dirs(path) 
  idx <- grep("\\d*\\.\\d*", dirs)
  if (length(idx)) {
    for (ii in idx) {
      rversion_current <- basename(dirs[ii])
      if (rversion_current != rversion) {
        path_new <- gsub(rversion_current, rversion, dirs[ii], fixed = TRUE)
        file.rename(from = dirs[ii], to = path_new)
      }
    }
  }
}

withConditionalWorkingDirectory <- function(code) {
  wd <- getwd()
  if (!length(grep("/tests/testthat$", wd))) {
    setwd("tests/testthat")
  }
  on.exit(setwd(wd))
  force(code)
}
