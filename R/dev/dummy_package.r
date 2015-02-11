root_1 <- "q:/home/wsp/rapp2/lcran"
repo <- PackageRepository$new(root_1)
repo$ensure()

## In dummy package project //
devtools::build(path = file.path(root_1, "src/contrib")
devtools::build(path = file.path(root_1, "bin/windows/contrib/3.2", binary = TRUE)

repo$refresh()

root_2 <- "q:/home/wsp/rapp2/lcran_2"
repo <- PackageRepository$new(root_2)
repo$refresh()
