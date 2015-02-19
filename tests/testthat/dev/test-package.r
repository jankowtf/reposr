## Create instance //
repo <- PackageRepository$new(file.path(Sys.getenv("HOME"), "code/lcran"))

## Ensure existence //
repo$ensure()

## Register //
getOption("repos")
repo$register()

## Build package directly into repo //
repo$buildInto()

## Pull all dependencies from CRAN to create a local repo snapshot //
repo$pull()

## Browse repo conten //
repo$browse("win.binary")
