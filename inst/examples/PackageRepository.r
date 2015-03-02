##------------------------------------------------------------------------------
## Preliminaries //
##------------------------------------------------------------------------------

## Choose root directory //
root <- file.path(tempdir(), "cran")

## Create instance //
repo <- PackageRepository$new(root)

## Investigate object structure //
repo
print(repo, full = TRUE)

## Most important fields //
repo$root
repo$root_archive
repo[[getOption("pkgType")]]
repo$win.binary
repo$mac.binary
repo$source

repo$scheme
repo$normalize 
repo$detect_scheme 

##------------------------------------------------------------------------------
## Top-level methods (in alphabetical order) //
##------------------------------------------------------------------------------

#######
## A ##
#######

## As URL path //
repo$asUrl()
repo$asUrl(scheme = "http")
repo$asUrl(scheme = "ftp")

repo$asUrl(archive = TRUE)
repo$asUrl(scheme = "http", archive = TRUE)
repo$asUrl(scheme = "ftp", archive = TRUE)

#######
## B ##
#######

## Build into //
repo$ensure()
repo$buildInto()

## Check how repo has changed:
repo$browse(getOption("pkgType"))

## Browse //
repo$ensure()
repo$browse()
repo$browse("win.binary")
repo$browse("mac.binary")
repo$browse("source")

repo$ensure(archive = TRUE)
repo$browse(archive = TRUE)
repo$browse("win.binary", archive = TRUE)
repo$browse("mac.binary", archive = TRUE)
repo$browse("source", archive = TRUE)

#######
## C ##
#######

## Clean //
repo$clean()

#######
## D ##
#######

## Delete //
repo$delete()
repo$ensure()
repo$delete(ask = FALSE)
repo$delete(ask = FALSE)
## --> trying to delete a non-existing repo

repo$delete(archive = TRUE)
repo$ensure(archive = TRUE)
repo$delete(archive = TRUE, ask = FALSE)

#######
## E ##
#######

## Ensure //
repo$ensure()
repo$ensure(overwrite = TRUE)

repo$ensure(archive = TRUE)

## Ensure existence //
file.exists(repo$root)
repo$ensure()
file.exists(repo$root)

file.exists(repo$root_archive)
repo$ensure(archive = TRUE)
file.exists(repo$root_archive)

## Exists //
repo$exists()
repo$ensure()
repo$exists()

repo$exists(archive = TRUE)
repo$ensure(archive = TRUE)
repo$exists(archive = TRUE)

#######
## H ##
#######

## Has any packages //
repo$hasAny()
repo$hasAny(atomic = FALSE)

## Has packages //
repo$has()
repo$buildInto()
repo$has()
repo$has(atomic = FALSE)
repo$has(type = "source", atomic = FALSE)
repo$has(type = "mac.binary", atomic = FALSE)
repo$has(type = "win.binary", atomic = FALSE)

repo$has("devtools")
repo$has(c("testthat", "devtools"))

#######
## R ##
#######

## Refresh //
repo$refresh()

## Register //
repo$register()

## Remove packages //
repo$remove()

## Reset //
repo$reset()

#######
## S ##
#######

## Show //
repo$show()
## --> if empty data frame call '$buildInto()` before
# repo$buildInto()
repo$show()
