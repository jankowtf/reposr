withConditionalWorkingDirectory <- function(code) {
  wd <- getwd()
  if (!length(grep("/tests/testthat$", wd))) {
    setwd("tests/testthat")
  }
  on.exit(setwd(wd))
  force(code)
}

##------------------------------------------------------------------------------
context("scratch")
##------------------------------------------------------------------------------

test_that("packrat", {

  repo <- PackageRepository$new(file.path(Sys.getenv("HOME"), 
    "code/cran_rappster"))
#   repo$register()
  getOption("repos")
  
  repo <- PackageRepository$new(packrat = TRUE)
  expect_identical(repo$root, file.path(getwd(), "packrat/cran"))
#   repo$delete(ask = FALSE)
  repo$ensure()
  repo$pull() ## --> error
  repo$pull(atomic_repos = TRUE)

  self=repo
  private = environment(self$ensure)$private
  
  path_src <- file.path(tempdir(), "test.txt")
  write("Hello World!", file = path_src)
  path_tgt <- file.path(tempdir(), "test_symlink.txt")
  shell(sprintf("mklink /H %s %s", 
    normalizePath(path_tgt, mustWork = FALSE),
    normalizePath(path_src)
  ))
  write("HELLO WORLD!", file = path_src, append = TRUE)
    
  path_tgt_2 <- file.path(tempdir(), "test_symlink_2.txt")
  shell(sprintf("mklink /D %s %s", 
    normalizePath(path_tgt_2, mustWork = FALSE),
    normalizePath(path_src)
  ))
  
  shell(sprintf("runas mklink /D %s %s", 
    normalizePath(path_tgt_2, mustWork = FALSE),
    normalizePath(path_src)
  ))
  
})

