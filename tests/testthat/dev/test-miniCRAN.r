rversion <- paste(
  R.version$major, 
  unlist(strsplit(R.version$minor, split="\\."))[2], sep="."
)

withConditionalWorkingDirectory <- function(code) {
  wd <- getwd()
  if (!length(grep("/tests/testthat$", wd))) {
    setwd("tests/testthat")
  }
  on.exit(setwd(wd))
  force(code)
}

# install.packages("miniCRAN")
require("miniCRAN")
pkgs <- c("chron")
pkgDep(pkgs) 
p <- makeDepGraph(pkgs, enhances = TRUE)
# ?plot.pkgDepGraph 
plot(p)

options(repos = c(CRAN = "http://cran.at.r-project.org/"))

pkgs <- c("data.table", "dplyr", "ggvis", "tidyr", "ggplot2")

pkgDep(pkgs, enhances=TRUE)

p <- makeDepGraph(pkgs, enhances = TRUE)

set.seed(1)
plot(p, cex=1, vertex.size=15)

pth <- file.path(Sys.getenv("HOME"), "code/cran_2")
# dir.create(pth)
# makeRepo(pkgDep(pkgs), path=pth, download=TRUE) 

repo <- PackageRepository$new(file.path(Sys.getenv("HOME"), "code/cran_2"))
repo$ensure()
repo$buildInto()
repo$pull()
getOption("repos")

##-----

repo <- PackageRepository$new()
repo$ensure()
repo$buildInto()
repo$show()

repo$register(name = "LCRAN")

getOption("repos")
pkgs <- c("reposr", "chron")
pkgs <- "reposr"
repo$dependsOn()
repo$visualizeDependencies()
repo$visualizeDependencies(export = "test.png")
repo$visualizeDependencies(export = "test.svg")




##------------------------------------------------------------------------------
context("PackageRepository/constructor")
##------------------------------------------------------------------------------

test_that("PackageRepository/constructor/bare", {

  expect_is(res <- PackageRepository$new(), "PackageRepository")
  expect_true(!is.null(res$root))
  expect_true(!is.null(res$type))
  expect_identical(res$type, "fs")
  
})

