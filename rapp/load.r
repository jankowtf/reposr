## Working directory //
path_wd <- "q:/home/wsp/rapp2/rapp.core.repos"
if (getwd() != path_wd) {
  setwd(path_wd)    
}

## Packages //
require("compiler")
require("devtools")
require("packrat")
require("testthat")

## Load //
packrat::on()
load_all(recompile=TRUE)

## Ensure examples //
# ensureExampleFiles()
#packrat::on()
#packrat::snapshot()
#.libPaths()
#packrat_mode()
