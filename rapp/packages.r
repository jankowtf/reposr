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
# install.packages("shiny")
# install.packages("httr")
# install.packages("CSS")
# install.packages("selectr")
# install.packages("devtools")

require("devtools")
require("packrat")
require("testthat")
packrat::off()

packrat::on()
packrat::off()
packrat::snapshot()
