rversion <- paste(
  R.version$major, 
  unlist(strsplit(R.version$minor, split="\\."))[1], sep="."
)
.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}
withConditionalWorkingDirectory <- function(code) {
  wd <- getwd()
  if (!length(grep("/tests/testthat$", wd))) {
    setwd("tests/testthat")
  }
  on.exit(setwd(wd))
  force(code)
}
#   withConditionalWorkingDirectory({
#     root <- "data/lcran"
#     repo <- PackageRepository$new(root = root)
#     repo$ensure()
#     repo$buildInto()
#   })    

##------------------------------------------------------------------------------
context("PackageRepository/constructor")
##------------------------------------------------------------------------------

test_that("PackageRepository/constructor/bare", {

  expect_is(res <- PackageRepository$new(), "PackageRepository")
  expect_true(!is.null(res$root))
  expect_true(!is.null(res$scheme))
  expect_identical(res$scheme, "none")
  
})

test_that("PackageRepository/constructor/types", {
  
  expect_is(res <- PackageRepository$new(root = "a", 
    normalize = FALSE, detect_scheme = FALSE), "PackageRepository")
  
  res$scheme <- "file"
  expect_identical(res$root, "file:///a")
  
  res$scheme <- "http"
  expect_identical(res$root, "http://a")
  
  res$scheme <- "ftp"
  expect_identical(res$root, "ftp://a")
  
  res$scheme <- "none"
  expect_identical(res$root, "a")
  
  res$scheme <- "invalid_type"
  expect_error(res$root)
  
})

test_that("PackageRepository/constructor/types", {
  
  expect_is(res <- PackageRepository$new(root = "a", 
    normalize = FALSE), "PackageRepository")
  
  res$scheme <- "file"
  expect_identical(res$root, "a")
  
  res$scheme <- "http"
  expect_identical(res$root, "a")
  
  res$scheme <- "ftp"
  expect_identical(res$root, "a")
  
  res$scheme <- "none"
  expect_identical(res$root, "a")
  
  res$scheme <- "invalid_type"
  expect_identical(res$root, "a")
  
})

test_that("PackageRepository/constructor/http", {
  
  root <- "http://cran.rstudio.com" 
  expect_is(res <- PackageRepository$new(root = root), "PackageRepository")
  expect_identical(res$root, root)
  
  res$scheme <- "file"
  expect_identical(res$root, root)
  
  res$scheme <- "http"
  expect_identical(res$root, root)
  
  res$scheme <- "ftp"
  expect_identical(res$root, root)
  
  res$scheme <- "none"
  expect_identical(res$root, root)
  
  res$scheme <- "invalid_type"
  expect_identical(res$root, root)
  
})

test_that("PackageRepository/constructor/http/no scheme derivation", {
  
  root <- "http://cran.rstudio.com" 
  root_noscheme <- gsub("http://", "", root)
  
  expect_is(res <- PackageRepository$new(root = root, detect_scheme = FALSE), 
    "PackageRepository")
  expect_identical(res$root, file.path(getwd(), root_noscheme))
  
  res$scheme <- "file"
  expect_identical(res$root, file.path("file://", getwd(), root_noscheme))
  
  res$scheme <- "http"
  expect_identical(res$root, file.path("http:/", root_noscheme))
  
  res$scheme <- "ftp"
  expect_identical(res$root, file.path("ftp:/", root_noscheme))
  
  res$scheme <- "none"
  expect_identical(res$root, file.path(getwd(), root_noscheme))
  
  res$scheme <- "invalid_type"
  expect_error(res$root)
  
})

test_that("PackageRepository/constructor/fields", {
  
  expect_is(res <- PackageRepository$new(
    root = "a/b/c", scheme = "none"), "PackageRepository")
  expect_identical(res$root, normalizePath("a/b/c", winslash = "/", 
    mustWork = FALSE))
  expect_identical(res$scheme, "none")
  
})

test_that("PackageRepository/constructor/no normalization", {
  
  expect_is(res <- PackageRepository$new(
    root = "a/b/c", normalize = FALSE), "PackageRepository")
  expect_identical(res$root, "a/b/c")

})

withConditionalWorkingDirectory(
  test_that("PackageRepository/change root", {
    
    expect_is(res <- PackageRepository$new(), "PackageRepository")
    
    res$root <- "a"
    expect_identical(res$root, 
      normalizePath(file.path(getwd(), "a"), winslash = "/", mustWork = FALSE))
    
  })
)

##------------------------------------------------------------------------------
context("PackageRepository/subdirectories")
##------------------------------------------------------------------------------

withConditionalWorkingDirectory(
  test_that("PackageRepository/subdirectories", {
    
    expect_is(res <- PackageRepository$new(), "PackageRepository")
    
    expect_identical(res$mac.binary,
      structure(file.path(getwd(), "lcran/bin/macosx/contrib", rversion), 
                names = "mac.binary"))
    expect_identical(res$win.binary,
      structure(file.path(getwd(), "lcran/bin/windows/contrib", rversion), 
                names = "win.binary"))
    expect_identical(res$source,
      structure(file.path(getwd(), "lcran/src/contrib"), names = "source"))
  })
)

################################################################################
## Public methods //
################################################################################

##------------------------------------------------------------------------------
context("PackageRepository/as URL")
##------------------------------------------------------------------------------

test_that("PackageRepository/as URL", {
  
  repo <- PackageRepository$new()
  expect_identical(repo$asUrl(), file.path("file://", repo$root))
  expect_identical(repo$asUrl("file"), file.path("file://", repo$root))
  expect_identical(repo$asUrl("http"), file.path("http://", repo$root))
  expect_identical(repo$asUrl("ftp"), file.path("ftp://", repo$root))
  
})

test_that("PackageRepository/as URL/archive", {
  
  repo <- PackageRepository$new()
  root <- "root_archive"
  expect_identical(repo$asUrl(archive = TRUE), file.path("file://", repo[[root]]))
  expect_identical(repo$asUrl("file", archive = TRUE), 
    file.path("file://", repo[[root]]))
  expect_identical(repo$asUrl("http", archive = TRUE), 
    file.path("http://", repo[[root]]))
  expect_identical(repo$asUrl("ftp", archive = TRUE),
    file.path("ftp://", repo[[root]]))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/browse")
##------------------------------------------------------------------------------

if (basename(getwd()) != "testthat") {
  test_that("PackageRepository/browse", {
    
    root <- file.path(tempdir(), "lcran")
    repo <- PackageRepository$new(root)
    repo$ensure(overwrite = TRUE, ask = FALSE)
    
    expect_true(repo$browse())
    expect_true(repo$browse("win.binary"))
    expect_true(repo$browse("mac.binary"))
    expect_true(repo$browse("source"))
    expect_error(repo$browse(c("win.binary", "source")))
    expect_error(repo$browse("invalid"))
    repo$delete(ask = FALSE)
    
  })
  
  test_that("PackageRepository/browse/strict", {
    
    root <- file.path(tempdir(), "lcran")
    repo <- PackageRepository$new(root)
    repo$delete(ask = FALSE)
    expect_false(repo$browse())
    expect_message(expect_false(repo$browse(strict = 1)))
    expect_warning(expect_false(repo$browse(strict = 2)))
    expect_error(expect_false(repo$browse(strict = 3)))
    
  })
  
  test_that("PackageRepository/browse/archive", {
    
    root <- file.path(tempdir(), "lcran")
    repo <- PackageRepository$new(root)
    repo$ensure(archive = TRUE, overwrite = TRUE, ask = FALSE)
    
    expect_true(repo$browse(archive = TRUE))
    expect_true(repo$browse("win.binary", archive = TRUE))
    expect_true(repo$browse("mac.binary", archive = TRUE))
    expect_true(repo$browse("source", archive = TRUE))
    expect_error(repo$browse(c("win.binary", "source"), archive = TRUE))
    expect_error(repo$browse("invalid", archive = TRUE))
    repo$delete(ask = FALSE)
    
  })
}

##------------------------------------------------------------------------------
context("PackageRepository/build into")
##------------------------------------------------------------------------------

test_that("PackageRepository/build into", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  expect_error(repo$buildInto())
  expect_true(repo$buildInto(ensure = TRUE))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/clean")
##------------------------------------------------------------------------------

test_that("PackageRepository/clean", {
    
  withConditionalWorkingDirectory(
    file.copy("data/lcran", tempdir(), recursive = TRUE, overwrite = TRUE)
  )
  repo <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = repo)
  expect_true(repo$clean())
  expect_true(file.exists(repo$root_archive))
  expect_true(length(list.files(repo$root_archive, 
    recursive = TRUE, pattern = "dummy")) == 4)
  repo$delete(ask = FALSE)
  repo$delete(archive = TRUE, ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/delete")
##------------------------------------------------------------------------------

test_that("PackageRepository/delete", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$ensure()
  expect_true(repo$delete(ask = FALSE))
#   Sys.sleep(1)
# print(repo$root)
# print(file.exists(repo$root))  
  expect_false(file.exists(repo$root))
  
})

test_that("PackageRepository/delete/archive", {
  
  repo_archive <- file.path(tempdir(), "lcran_archive")
  dir.create(repo_archive)
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$ensure()
  expect_true(repo$delete(archive = TRUE, ask = FALSE))
  expect_false(file.exists(repo_archive))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/depends on")
##------------------------------------------------------------------------------

test_that("PackageRepository/depends on", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran_3")
  )
#   file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
#   root <- file.path(tempdir(), "lcran_3")
  repo <- PackageRepository$new(root = root)
  
  options("repos" = c(CRAN = "http://cran.rstudio.com", 
    LCRAN = repo$asUrl()))
  ## --> necessary for global test run
  
  expect_true(length(repo$dependsOn()) > 0)
  expect_true(length(repo$dependsOn(local_only = TRUE)) < 6)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/ensure")
##------------------------------------------------------------------------------

test_that("PackageRepository/ensure", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  expect_true(repo$ensure())
  expect_true(file.exists(repo$root))
  
  ## Subdirs //
  expect_true(all(file.exists(
    file.path(repo$mac.binary, c("PACKAGES", "PACKAGES.gz")))))
  expect_true(file.exists(repo$source))
  expect_true(all(file.exists(
    file.path(repo$source, c("PACKAGES", "PACKAGES.gz")))))
  expect_true(file.exists(repo$win.binary))
  expect_true(all(file.exists(
    file.path(repo$win.binary, c("PACKAGES", "PACKAGES.gz")))))
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/ensure/overwrite", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  expect_true(repo$ensure())
  if (interactive()) {
    repo$ensure(overwrite = TRUE)
  }
  expect_true(repo$ensure(overwrite = TRUE, ask = FALSE))
  
})

test_that("PackageRepository/ensure/archive", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  expect_true(repo$ensure(archive = TRUE))
  expect_true(file.exists(repo$root_archive))
  repo$delete(archive = TRUE, ask = FALSE)

})

##------------------------------------------------------------------------------
context("PackageRepository/exists")
##------------------------------------------------------------------------------

test_that("PackageRepository/exists", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  expect_false(repo$exists())
  repo$ensure()
  expect_true(repo$exists())
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/exists/archive", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  expect_false(repo$exists(archive = TRUE))
  repo$ensure(archive = TRUE)
  expect_true(repo$exists(archive = TRUE))
  repo$delete(archive = TRUE, ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/export")
##------------------------------------------------------------------------------

test_that("PackageRepository/export/entire repo", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$ensure()
  to <- file.path(tempdir(), "lcran_2")
  expect_true(repo$export(to = to))
  expect_true(file.exists(to))
  repo$delete(ask = FALSE)
  unlink(to, recursive = TRUE)
  
})

test_that("PackageRepository/export/package", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran_3")
  )
  file.copy(root, tempdir(), recursive = TRUE)
  repo <- PackageRepository$new(root = file.path(tempdir(), "lcran_3"))
  to <- file.path(tempdir(), "lcran_new")
  expect_true(all(res <- repo$export(pkg = "reposr", to = to)))
  expect_true(all(sapply(names(res), file.exists)))
  repo$delete(ask = FALSE)
  unlink(to, recursive = TRUE)
  
})

test_that("PackageRepository/export/packages", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran_4")
  )
  file.copy(root, tempdir(), recursive = TRUE)
  repo <- PackageRepository$new(root = file.path(tempdir(), "lcran_4"))
  to <- file.path(tempdir(), "lcran_new")
  
  expect_true(all(res <- repo$export(pkg = c("reposr", "R6"), to = to)))
  expect_true(all(sapply(names(res), file.exists)))
  repo$delete(ask = FALSE)
  unlink(to, recursive = TRUE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/has any")
##------------------------------------------------------------------------------

test_that("PackageRepository/has any", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran")
  )
  repo <- PackageRepository$new(root = root)
  
  expect_true(repo$hasAny())
  expect_identical(repo$hasAny(atomic = TRUE),
    c(mac.binary = FALSE, source = TRUE, win.binary = TRUE))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/has package")
##------------------------------------------------------------------------------

test_that("PackageRepository/has packages/single", {
  
  withConditionalWorkingDirectory({
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root) 
  expect_true(repo$has(pkg = "dummy"))
  expect_identical(repo$has(pkg = "dummy", atomic = FALSE),
    structure(list(c(dummy = TRUE)), names = getOption("pkgType")))
  expect_identical(
    repo$has(pkg = "dummy", type = "source", atomic = FALSE),
    structure(list(c(dummy = TRUE)), names = "source")
  )
  expect_identical(
    repo$has(pkg = "dummy", type = "mac.binary", atomic = FALSE),
    structure(list(c(dummy = FALSE)), names = "mac.binary")
  )
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/has package/multiple", {
  
  withConditionalWorkingDirectory({
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root) 
  pkg <- rep("dummy", 2)
  expect_identical(repo$has(pkg = pkg),
    structure(rep(TRUE, 2), names = rep("dummy", 2)))
  expect_identical(repo$has(pkg = pkg, atomic = FALSE),
    structure(list(rep(c(dummy = TRUE), 2)), names = getOption("pkgType")))
  expect_identical(
    repo$has(pkg = pkg, type = "source", atomic = FALSE),
    structure(list(rep(c(dummy = TRUE), 2)), names = "source")
  )
  expect_identical(
    repo$has(pkg = pkg, type = "mac.binary", atomic = FALSE),
    structure(list(rep(c(dummy = FALSE), 2)), names = "mac.binary")
  )
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/pull")
##------------------------------------------------------------------------------

test_that("PackageRepository/pull", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran_4")
  )
  file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
  repo <- PackageRepository$new(root = file.path(tempdir(), "lcran_4"))
  
  repo_rappster <- PackageRepository$new(
    root = file.path(Sys.getenv("HOME"), "code/cran_rappster"))
  repo_rappster$show()  
  repo_rappster$register()
#   repo <- PackageRepository$new(root = root)
#   repo$buildInto(ensure = TRUE)
#   repo$register()
#   repo$dependsOn()
#   self=repo
  expect_true(repo$pull())
  expect_true(length(list.files(repo$source)) >= 25)
  repo_rappster$unregister()
  repo$unregister()
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/refresh")
##------------------------------------------------------------------------------

test_that("PackageRepository/refresh", {
  
  root <- file.path(tempdir(), "lcran")
  expect_is(repo <- PackageRepository$new(root = root), "PackageRepository")
  expect_true(repo$ensure())
  expect_true(repo$refresh())
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/register")
##------------------------------------------------------------------------------

test_that("PackageRepository/register", {
  
  opts <- getOption("repos")
  repo <- PackageRepository$new()
  expect_true(repo$register())
  expect_identical(getOption("repos")[1], 
    structure(repo$asUrl(), names = "CRAN"))
  expect_true(length(environment(repo$ensure)$private$.roption_repo_cache) > 0)
  
  repo <- PackageRepository$new(root = file.path(tempdir(), "lcran"))
  expect_true(repo$register(before_cran = FALSE))
  opts_2 <- getOption("repos")
  expect_identical(opts_2[length(opts_2)], structure(repo$asUrl(), 
    names = "CRAN"))
  
  on.exit(options("repos" = opts))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/remove packages")
##------------------------------------------------------------------------------

test_that("PackageRepository/remove/all types", {
  
  withConditionalWorkingDirectory({
    file.copy("data/lcran_2", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "lcran_2")
  repo <- PackageRepository$new(root = root) 
  expect_true(repo$remove(pkg = "dummy", ask = FALSE))
# self=repo  
# private=environment(self$ensure)$private

  expect_false(length(
    list.files(repo$root, recursive = TRUE, pattern = "dummy")) > 0)
  expect_false(repo$remove(pkg = "dummy", ask = FALSE))
  repo$delete(ask = FALSE)

})

test_that("PackageRepository/remove/specific types", {
  
  withConditionalWorkingDirectory({
    file.copy("data/lcran_2", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "lcran_2")
  repo <- PackageRepository$new(root = root) 
  expect_true(repo$remove(pkg = "dummy", 
    type = "source", ask = FALSE))
  expect_true(length(
    list.files(repo$root, recursive = TRUE, pattern = "dummy")) > 0)
  expect_true(repo$remove(pkg = "dummy", 
    type = getOption("pkgType"), ask = FALSE))
  expect_false(length(
    list.files(repo$root, recursive = TRUE, pattern = "dummy")) > 0)
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/reset")
##------------------------------------------------------------------------------

test_that("PackageRepository/reset", {
  
  root <- file.path(tempdir(), "lcran")
  withConditionalWorkingDirectory(
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  )
  repo <- PackageRepository$new(root = root)
  expect_true(repo$reset(ask = FALSE))
  expect_true(!length(list.files(repo$root, recursive = TRUE, pattern = "dummy")))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/show")
##------------------------------------------------------------------------------

test_that("PackageRepository/show", {
  
  withConditionalWorkingDirectory(
    root <- file.path(getwd(), "data/lcran_2")
  )
  repo <- PackageRepository$new(root = root)
  expect_true(length(index <- repo$show()) > 0)
  expect_equal(index$Package, "dummy")    
  
})

##------------------------------------------------------------------------------
context("PackageRepository/unregister")
##------------------------------------------------------------------------------

test_that("PackageRepository/unregister", {
  
  withConditionalWorkingDirectory(
    root <- file.path(getwd(), "data/lcran_2")
  )
  repo <- PackageRepository$new(root)
  repo$register()
  expect_true(length(getOption("repos")) == 3)
  expect_true(repo$unregister())
  expect_true(length(getOption("repos")) == 2)
  
})

test_that("PackageRepository/unregister/reset", {
  
  withConditionalWorkingDirectory(
    root <- file.path(getwd(), "data/lcran_2")
  )
  repo_1 <- PackageRepository$new(root)
  repo_1$register()  
  withConditionalWorkingDirectory(
    root <- file.path(getwd(), "data/lcran_3")
  )
  repo_2 <- PackageRepository$new(root)
  repo_2$register()  

#   expect_true(length(getOption("repos")) == 4)
  ## --> somehow `CRANextra' does not seem to be included in the batch run
  expect_true(length(getOption("repos")) >= 3)
  expect_true(repo_1$unregister(reset = TRUE))
  expect_true(length(getOption("repos")) == 2)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/visualizeDependencies")
##------------------------------------------------------------------------------

test_that("PackageRepository/visualizeDependencies", {
  
  withConditionalWorkingDirectory(    
    root <- file.path(getwd(), "data/lcran_3")
  )
  repo <- PackageRepository$new(root)
  repo$register()
  expect_null(repo$visualizeDependencies())
  withConditionalWorkingDirectory(    
    expect_is(res <- repo$visualizeDependencies(
      export = file.path(getwd(), "data/test.png")), "character")
  )
  expect_true(file.exists(res))
  unlink(res, force = TRUE)
  withConditionalWorkingDirectory(    
    expect_is(res <- repo$visualizeDependencies(
      export = file.path(getwd(), "data/test.svg")), "character")
  )
  expect_true(file.exists(res))
  unlink(res, force = TRUE)
  withConditionalWorkingDirectory(    
    expect_is(res <- repo$visualizeDependencies(
      export = file.path(getwd(), "data/test.pdf")), "character")
  )
  expect_true(file.exists(res))
  unlink(res, force = TRUE)
  
})

################################################################################
## Private methods //
################################################################################

##------------------------------------------------------------------------------
context("PackageRepository/private/derive root")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/derive root", {
  
  repo <- PackageRepository$new(root = file.path(tempdir(), "lcran"))
  private <- environment(repo$ensure)$private
  expect_identical(
    private$deriveRoot(
      "file:///c:/temp/cran/bin/windows/contrib/3.1", type = "win.binary"
    ),
    "c:/temp/cran"
  )
  expect_identical(
    private$deriveRoot(
      "file:///c:/temp/cran/bin/macosx/contrib/3.1", type = "mac.binary"
    ),
    "c:/temp/cran"
  )
  expect_identical(
    private$deriveRoot(
      "file:///c:/temp/cran/src/contrib", type = "source"
    ),
    "c:/temp/cran"
  )
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/detect scheme")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/detect scheme", {
  
  root <- "http://cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_identical(private$detectScheme(repo$root), "http")
  
  root <- "ftp://cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_identical(private$detectScheme(repo$root), "ftp")
  
  root <- "file://cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_identical(private$detectScheme(repo$root), "file")
  
  root <- "cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_identical(private$detectScheme(repo$root), "none")
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/ensureIndexFiles")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/ensureIndexFiles", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_error(private$ensureIndexFiles())
  repo$ensure()
  expect_true(all(private$ensureIndexFiles()))
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/private/ensureIndexFiles/archive", {
  
  root <- file.path(tempdir(), "lcran")
  repo <- PackageRepository$new(root = root)
  repo$delete(archive = TRUE, ask = FALSE)
  private <- environment(repo$ensure)$private
  expect_error(private$ensureIndexFiles(archive = TRUE))  
  repo$ensure(archive = TRUE)
  expect_true(all(private$ensureIndexFiles(archive = TRUE)))
  repo$delete(archive = TRUE, ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/getLatestPackages")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/getLatestPackages", {
  
  root <- file.path(tempdir(), "lcran")
  withConditionalWorkingDirectory(
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  )
  repo <- PackageRepository$new(root = root)
  priv <- environment(repo$ensure)$private
  expect_is(res <- priv$getLatestPackages(refresh = FALSE), "list")
  expect_identical(res[[getOption("pkgType")]],
    data.frame(
     name = "dummy",
     version = "1.2",
     type = getOption("pkgType"),
     pattern = "/dummy_1.2\\.",
     fname = "dummy_1.2",
     fpath = file.path(repo[[getOption("pkgType")]], "dummy_1.2.zip"),
     index = as.integer(3),
     stringsAsFactors = FALSE
    )
  )
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/getOldPackages")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/getOldPackages", {
  
  root <- file.path(tempdir(), "lcran")
  withConditionalWorkingDirectory(
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  )
  repo <- PackageRepository$new(root = root)
  priv <- environment(repo$ensure)$private
  expect_is(res <- priv$getOldPackages(refresh = FALSE), "list")
  expect_identical(res[[getOption("pkgType")]][1,],
    data.frame(
     name = "dummy",
     version = "1.0",
     type = getOption("pkgType"),
     fpath = file.path(repo[[getOption("pkgType")]], "dummy_1.0.zip"),
     stringsAsFactors = FALSE
    )
  )
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/archivePackages")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/archivePackages", {
  
  root <- file.path(tempdir(), "lcran")
  withConditionalWorkingDirectory(
    file.copy("data/lcran", tempdir(), recursive = TRUE)
  )
  repo <- PackageRepository$new(root = root)
# self = repo  
  private <- environment(repo$ensure)$private
  expect_is(res <- private$archivePackages(refresh = FALSE), "list")
  expect_identical(res[[getOption("pkgType")]], c(dummy_1.0 = TRUE, dummy_1.1 = TRUE))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/parse index file")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/parse index file", {
  
  withConditionalWorkingDirectory(
    root <- file.path(getwd(), "data/lcran")
  )
  expect_is(repo <- PackageRepository$new(root = root), "PackageRepository")
  private <- environment(repo$ensure)$private
  expect_true(length(index <- private$parseIndexFile()) > 0)
  expect_equal(index$Package, "dummy")    
  
})

# res <- asRepository()
# res$root
# self = repo
# private = environment(self$ensure)$private
# wd_0 <- setwd("tests/testthat")
# setwd(wd_0)
# print(warnings())