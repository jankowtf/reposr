## Working directory //
path_wd <- "q:/home/wsp/rapp2/reposr"
if (getwd() != path_wd) {
  setwd(path_wd)    
}

require("testthat")
test()