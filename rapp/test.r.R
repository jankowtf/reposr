## Working directory //
path_wd <- "q:/home/wsp/rapp2/repositr"
if (getwd() != path_wd) {
  setwd(path_wd)    
}

require("testthat")
test()