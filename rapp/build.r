## Working directory //
path_wd <- "q:/home/wsp/rapp2/repor"
if (getwd() != path_wd) {
  setwd(path_wd)    
}

## Packages //
require("devtools")
require("roxygen2")

## Unload //
unload()

## Roxygenize //
roxygenize()

## Build //
build(binary=TRUE, path="rapp")
# build(path="rapp")

## Install //
# install()

## Require //
# require("rapp.split")


