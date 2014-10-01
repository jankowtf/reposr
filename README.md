repositr (v0.1.4)
======

Local Package Repository Management

## Installation

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/descriptionr")
devtools::install_github("Rappster/repositr")
require("repositr")
```

## Ensure repository

Ensure the existence of a local package repository:

```
ensureRepository(repos = file.path(tempdir(), "repos"))
# C:/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos 
#                                             TRUE 
```

## Ensure local repository infrastructure

Ensure the existence of a local package repository infrastructure consisting of a *global* and *package-and-version-specific* repositories below `repos_root`.

This has been mainly designed for systematically managing repositories of own packages.

```
ensureRepositoryInfrastructure(repos_home = file.path(tempdir(), "repos",
  pkg_name = "test.package", pkg_version = "0.1"))
#                  C:/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/test.package/0.1/global 
#                                                                                      TRUE 
# C:/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/test.package/0.1/packages/repositr/0.1.4 
#                                                                                      TRUE   
```

## Build a package into local repository infrastructure

Prerequisites:

- This requires that your working directory points to the root directory of a valid package project.
- It also requires that you installed the [Rtools](http://cran.at.r-project.org/bin/windows/Rtools/) and that the `bin` directory is included in your Windows `PATH` variable.

The functionality can be illustrated by building the test package used for unit testing in `{repositr-package}/tests/testthat/data/test.package`

```
path_pkg <- "path/to/repositr/package/tests/testthat/data/test.package"
if (!file.exists(path_pkg)) {
  stop(paste0("Invalid directory: ", path_pkg))
}

## Temporarily change working directory y//
wd_0 <- setwd(path_pkg)

## Build test package //
# buildIntoRepositoryInfrastructure(repos_home = file.path(tempdir(), "repos",
#   binary = TRUE))
# Updating test.package documentation
# Loading test.package
# Writing test.package.Rd
# "Q:/home/apps/RAPPTO~1/apps/r/R-31~1.1/bin/x64/R" --vanilla CMD build  \
#   "Q:\home\wsp\rapp2\repositr\tests\testthat\data\test.package" --no-manual  \
#   --no-resave-data 
# 
# * checking for file 'Q:\home\wsp\rapp2\repositr\tests\testthat\data\test.package/DESCRIPTION' ... OK
# * preparing 'test.package':
# * checking DESCRIPTION meta-information ... OK
# * checking for LF line-endings in source and make files
# * checking for empty or unneeded directories
# * looking to see if a 'data/datalist' file should be added
# * building 'test.package_1.0.tar.gz'
# 
# cygwin warning:
#   MS-DOS style path detected: C:/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/TRUE/global/src/contrib/test.package_1.0.tar.gz
#   Preferred POSIX equivalent is: /cygdrive/c/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/TRUE/global/src/contrib/test.package_1.0.tar.gz
#   CYGWIN environment variable option "nodosfilewarning" turns off this warning.
#   Consult the user's guide for more details about POSIX paths:
#     http://cygwin.com/cygwin-ug-net/using.html#using-pathnames
# cygwin warning:
#   MS-DOS style path detected: C:/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/TRUE/packages/test.package/1.0/src/contrib/test.package_1.0.tar.gz
#   Preferred POSIX equivalent is: /cygdrive/c/Users/jat/AppData/Local/Temp/Rtmp8KcUF1/repos/TRUE/packages/test.package/1.0/src/contrib/test.package_1.0.tar.gz
#   CYGWIN environment variable option "nodosfilewarning" turns off this warning.
#   Consult the user's guide for more details about POSIX paths:
#     http://cygwin.com/cygwin-ug-net/using.html#using-pathnames
#                    C:\\Users\\jat\\AppData\\Local\\Temp\\Rtmp8KcUF1/repos/TRUE/global 
#                                                                                  TRUE 
# C:\\Users\\jat\\AppData\\Local\\Temp\\Rtmp8KcUF1/repos/TRUE/packages/test.package/1.0 
#                                                                                  TRUE   

## Clean up //
setwd(wd_0)
```
