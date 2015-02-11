install.packages("miniCRAN")
library("miniCRAN")

# options(repos=c(CRAN="http://cran.at.r-project.org/"))
# install.packages("miniCRAN", type = "source")

library("miniCRAN")
sessionInfo() 

pkgs <- c("chron")

miniCRAN::pkgDep(pkgs)

p <- miniCRAN::makeDepGraph(pkgs, enhances = TRUE)
?plot.pkgDepGraph

set.seed(20140917)
plot(p, cex=1.5, vertex.size=15) 

##

pkgs <- c("data.table", "dplyr", "ggvis", "tidyr", "ggplot2")
# pkgDep(pkgs, enhances=TRUE)

## Dependency plot //
p <- makeDepGraph(pkgs, enhances = TRUE)
set.seed(1)
plot(p, cex=1, vertex.size=15)

## Local repo //
pth <- file.path(Sys.getenv("HOME"), "miniCRAN")
dir.create(pth, recursive = TRUE, showWarnings = FALSE)
makeRepo(pkgDep(pkgs), path = pth, download = TRUE) 
makeRepo(pkgDep(pkgs), path = pth, type = "win.binary") 

################################################################################

# Create temporary project and set working directory

example_project <- paste0("~/checkpoint_example_project_", Sys.Date())

dir.create(example_project, recursive = TRUE)
oldwd <- setwd(example_project)


# Write dummy code file to project

cat("library(MASS)", "library(foreach)",
    sep="\n", 
    file="checkpoint_example_code.R")


# Create a checkpoint by specifying a snapshot date
# install.packages("checkpoint")
library(checkpoint)
checkpoint("2015-01-18")

# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")

# Check that library path is set to ~/.checkpoint
.libPaths()

# Check which packages are installed in checkpoint library
installed.packages()

# cleanup
unlink(example_project, recursive = TRUE)
setwd(oldwd)

## Creating own CRAN mirror 

# rsync -rtlzv --delete cran.r-project.org::CRAN D:/