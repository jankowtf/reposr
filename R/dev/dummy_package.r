##------------------------------------------------------------------------------
## In dummy package project //
##------------------------------------------------------------------------------

require("reposr")
reposr_home <- "q:/home/wsp/rapp2/reposr"

root_1 <- file.path(reposr_home, "tests/testthat/data/lcran")
repo <- PackageRepository$new(root_1)
# repo$root
repo$ensure()

devtools::build(path = file.path(root_1, "src/contrib"))
devtools::build(path = file.path(root_1, 
  "bin/windows/contrib/3.1", binary = TRUE))
# getwd()
repo$refresh()

##------------------------------------------------------------------------------
## In reposr package project //
##------------------------------------------------------------------------------

root_2 <- "tests/testthat/data/lcran_2"
repo <- PackageRepository$new(root_2)
repo$refresh()
repo$buildInto()
repo$clean(archive = FALSE)

root_3 <- "tests/testthat/data/lcran_3"
repo <- PackageRepository$new(root_3)
repo$ensure()
repo$buildInto()
repo$clean(archive = FALSE)
repo$refresh()
