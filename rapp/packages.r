Sys.setenv(language="en")

## Debug //
#update.packages(checkBuilt = TRUE)

## Global //
if (FALSE) {
    install.packages("devtools")
    require("devtools")
    devtools::install_github("rstudio/packrat")
}

## Project //
# install.packages("testthat")
# install.packages("roxygen2")
require("devtools")
require("packrat")
require("testthat")

packrat::on()
packrat::off()
packrat::snapshot()
