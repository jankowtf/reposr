## Working directory //
path_wd <- "q:/home/wsp/rapp2/rapp.core.repos"
if (getwd() != path_wd) {
  setwd(path_wd)    
}

require("testthat")
test()