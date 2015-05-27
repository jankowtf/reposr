rversion <- getRversion()
.cleanTempDir <- function(x) {
  if (grepl(basename(tempdir()), x)) {
    unlink(x, recursive = TRUE, force = TRUE)
  }
}
batch_test <- basename(getwd()) == "testthat"
# private <- getPrivate(repo)
# self <- repo

##------------------------------------------------------------------------------
context("PackageRepository/constructor")
##------------------------------------------------------------------------------

test_that("PackageRepository/constructor/bare", {

  expect_is(res <- PackageRepository$new(), "PackageRepository")
  expect_true(!is.null(res$root))
  expect_identical(res$root, file.path(getwd(), "cran"))
  expect_true(!is.null(res$scheme))
  expect_identical(res$scheme, "none")
  
})

test_that("PackageRepository/constructor/packrat", {
  
  repo <- PackageRepository$new(packrat = TRUE)
  expect_identical(repo$root, file.path(getwd(), "packrat/cran"))
  
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
      structure(file.path(getwd(), "cran/bin/macosx/contrib", rversion), 
                names = "mac.binary"))
    expect_identical(res$win.binary,
      structure(file.path(getwd(), "cran/bin/windows/contrib", rversion), 
                names = "win.binary"))
    expect_identical(res$source,
      structure(file.path(getwd(), "cran/src/contrib"), names = "source"))
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

test_that("PackageRepository/as URL/atomic", {
  
  repo <- PackageRepository$new()
  root <- "root_atomic"
  expect_identical(repo$asUrl(atomic = TRUE), file.path("file://", repo[[root]]))
  expect_identical(repo$asUrl("file", atomic = TRUE), 
    file.path("file://", repo[[root]]))
  expect_identical(repo$asUrl("http", atomic = TRUE), 
    file.path("http://", repo[[root]]))
  expect_identical(repo$asUrl("ftp", atomic = TRUE),
    file.path("ftp://", repo[[root]]))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/as non-URL")
##------------------------------------------------------------------------------

test_that("PackageRepository/as non-URL", {
  
  repo <- PackageRepository$new(file.path("file://", tempdir()))
  expect_identical(repo$asNonUrl(), normalizePath(tempdir(), winslash = "/"))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/browse")
##------------------------------------------------------------------------------

if (basename(getwd()) != "testthat") {
  test_that("PackageRepository/browse", {
    
    root <- file.path(tempdir(), "cran")
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
    
    root <- file.path(tempdir(), "cran")
    repo <- PackageRepository$new(root)
    repo$delete(ask = FALSE)
    expect_false(repo$browse())
    expect_message(expect_false(repo$browse(strict = 1)))
    expect_warning(expect_false(repo$browse(strict = 2)))
    expect_error(expect_false(repo$browse(strict = 3)))
    
  })
  
  test_that("PackageRepository/browse/archive", {
    
    root <- file.path(tempdir(), "cran")
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

if (!batch_test) {
  test_that("PackageRepository/build into", {
    
    root <- file.path(tempdir(), "cran")
    repo <- PackageRepository$new(root = root)
    expect_error(repo$buildInto())
    expect_true(repo$buildInto(ensure = TRUE))
    repo$delete(ask = FALSE)
    
  })
}

##------------------------------------------------------------------------------
context("PackageRepository/clean")
##------------------------------------------------------------------------------

test_that("PackageRepository/clean", {
    
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE, overwrite = TRUE)
  })
  repo <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = repo)
  expect_true(repo$clean())
  expect_true(file.exists(repo$root_atomic))
  expect_true(length(list.files(repo$root_atomic, 
    recursive = TRUE, pattern = "dummy")) == 4)
  repo$delete(ask = FALSE)
  repo$delete(atomic = TRUE, ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/delete")
##------------------------------------------------------------------------------

test_that("PackageRepository/delete", {
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$ensure()
  expect_true(repo$delete(ask = FALSE))
#   Sys.sleep(1)
# print(repo$root)
# print(file.exists(repo$root))  
  expect_false(file.exists(repo$root))
  
})

test_that("PackageRepository/delete/atomic", {
  
  repo_atomic <- file.path(tempdir(), "cran_atomic")
  dir.create(repo_atomic)
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$ensure()
  expect_true(repo$delete(atomic = TRUE, ask = FALSE))
  expect_false(file.exists(repo_atomic))
  
})

##------------------------------------------------------------------------------
context("PackageRepository/depends on")
##------------------------------------------------------------------------------

test_that("PackageRepository/depends on", {
  
  withConditionalWorkingDirectory({    
    adaptRversionNumber("data/cran_3")
    root <- file.path(getwd(), "data/cran_3")
  })
  repo <- PackageRepository$new(root = root)
  repo$register()
  
  if (batch_test) {
#     options("repos" = c(CRAN = "http://cran.rstudio.com"))
    options("repos" = c(CRAN = repo$asUrl(),
      CRAN = "http://cran.rstudio.com"))
    ## --> necessary for global test run
  }
  expect_true(length(repo$dependsOn()) > 0)
  expect_true(length(repo$dependsOn(local_only = TRUE)) < 6)
  repo$unregister(reset = TRUE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/ensure")
##------------------------------------------------------------------------------

test_that("PackageRepository/ensure", {
  
  root <- file.path(tempdir(), "cran")
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
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  expect_true(repo$ensure())
  if (interactive()) {
    repo$ensure(overwrite = TRUE)
  }
  expect_true(repo$ensure(overwrite = TRUE, ask = FALSE))
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/ensure/atomic", {
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  expect_true(repo$ensure(atomic = TRUE))
  expect_true(file.exists(repo$root_atomic))
  expect_true(file.exists(repo$root_atomic_pkg))
  expect_true(file.exists(repo$root_atomic_pkg_vsn))
  repo$delete(atomic = TRUE, ask = FALSE)

})

##------------------------------------------------------------------------------
context("PackageRepository/exists")
##------------------------------------------------------------------------------

test_that("PackageRepository/exists", {
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  expect_false(repo$exists())
  repo$ensure()
  expect_true(repo$exists())
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/exists/atomic", {
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  expect_false(repo$exists(atomic = TRUE))
  repo$ensure(atomic = TRUE)
  expect_true(repo$exists(atomic = TRUE))
  repo$delete(atomic = TRUE, ask = FALSE)
  
})

test_that("PackageRepository/exists/file", {
  
  root <- file.path("file://", tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  expect_false(repo$exists())
  repo$ensure()
  expect_true(repo$exists())
  repo$delete(ask = FALSE)
  
})

test_that("PackageRepository/exists/http", {
  
  root <- "http://cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  expect_error(repo$delete(ask = FALSE))
  expect_true(repo$exists())
  expect_error(repo$ensure())
  if (FALSE) {
    expect_true(repo$exists())
    repo$delete(ask = FALSE)
  }
  
})

test_that("PackageRepository/exists/ftp", {
  
#   root <- "ftp://cran.rstudio.com"
  root <- "ftp://cran.at.r-project.org/"
  repo <- PackageRepository$new(root = root)
  if (batch_test) {
    repos <- getOption("repos")
    repos["CRAN"] <- "http://cran.rstudio.com"
    options(repos = repos)
  }
  if (batch_test) {
    expect_false(repo$delete(ask = FALSE))
    expect_false(repo$exists())
  } else {
    expect_error(repo$delete(ask = FALSE))
    expect_true(repo$exists())
  }
  expect_error(repo$ensure())
  if (FALSE) {
    expect_true(repo$exists())
    repo$delete(ask = FALSE)
  }
  
})

##------------------------------------------------------------------------------
context("PackageRepository/push")
##------------------------------------------------------------------------------

test_that("PackageRepository/push/entire repo", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/repo_complete")
    root <- file.path(getwd(), "data/repo_complete")
  })
  repo <- PackageRepository$new(root = root)
  to <- file.path(tempdir(), "repo_pushed")
  expect_true(repo$push(to = to))
  expect_true(file.exists(to))
  expect_true(length(list.files(file.path(to, 
    "src/contrib"), pattern = "XML")) != 0)
  unlink(to, recursive = TRUE)
  
})

test_that("PackageRepository/push/package", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/repo_complete")
    root <- file.path(getwd(), "data/repo_complete")
  })
  repo <- PackageRepository$new(root = root)
  to <- file.path(tempdir(), "cran_new")
  expect_false(all(res <- repo$push(pkg = "reposr", to = to)))
  expect_true(all(res <- repo$push(pkg = "R6", to = to)))
  expect_true(all(sapply(names(res), file.exists)))
  unlink(to, recursive = TRUE)
  
})

test_that("PackageRepository/push/packages", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_4")
    root <- file.path(getwd(), "data/cran_4")
  })
  repo <- PackageRepository$new(root = root)
  to <- file.path(tempdir(), "cran_new")
  
  expect_true(all(res <- repo$push(pkg = c("reposr", "R6"), to = to)))
  expect_true(all(sapply(names(res), file.exists)))
  unlink(to, recursive = TRUE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/has any")
##------------------------------------------------------------------------------

test_that("PackageRepository/has any", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    root <- file.path(getwd(), "data/cran")
  })
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
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root) 
  expect_true(repo$has(pkg = "dummy"))
  expect_identical(repo$has(pkg = "dummy", atomic = FALSE),
    structure(list(c(dummy = TRUE)), names = .Platform$pkgType))
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
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root) 
  pkg <- rep("dummy", 2)
  expect_identical(repo$has(pkg = pkg),
    structure(rep(TRUE, 2), names = rep("dummy", 2)))
  expect_identical(repo$has(pkg = pkg, atomic = FALSE),
    structure(list(rep(c(dummy = TRUE), 2)), names = .Platform$pkgType))
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

if (!batch_test) {
  test_that("PackageRepository/pull", {
    
    withConditionalWorkingDirectory({
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    repo_rappster <- PackageRepository$new(
      root = file.path(Sys.getenv("HOME"), "code/cran_rappster"))
    repo_rappster$register()
  
    expect_true(repo$pull())
    expect_true(length(list.files(repo$source)) >= 25)
    repo_rappster$unregister()
    repo$unregister()
    repo$delete(ask = FALSE)
  
  })
  
  test_that("PackageRepository/pull/atomize", {
    
    withConditionalWorkingDirectory({  
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    repo_rappster <- PackageRepository$new(
      root = file.path(Sys.getenv("HOME"), "code/cran_rappster"))
    repo_rappster$register()
    expect_true(repo$pull(atomize = TRUE))
    expect_true(length(list.files(repo$root_atomic)) >= 23)
    expect_true(
      length(list.files(list.files(repo$root_atomic, full.names = TRUE))) >= 23
    )
    repo_rappster$unregister(reset = TRUE)
    repo$unregister(reset = TRUE)
    repo$delete(ask = FALSE)
    repo$delete(atomic = TRUE, ask = FALSE)
    
  })
  
  test_that("PackageRepository/pull/atomize/symlinks", {
    
    withConditionalWorkingDirectory({    
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    repo_rappster <- PackageRepository$new(
      root = file.path(Sys.getenv("HOME"), "code/cran_rappster"))
    repo_rappster$register()
    
    expect_true(repo$pull(atomize = TRUE, symlink = TRUE))
    expect_true(length(list.files(repo$root_atomic)) >= 23)
    expect_true(
      length(list.files(list.files(repo$root_atomic, full.names = TRUE))) >= 23
    )
    repo_rappster$unregister(reset = TRUE)
    repo$unregister(reset = TRUE)
    repo$delete(ask = FALSE)
    repo$delete(atomic = TRUE, ask = FALSE)
    
  })
  
  test_that("PackageRepository/pull/non-overwrite", {
    
    ## Generate up-to-date complete repository //
    if (FALSE) {
      repo_rappster <- PackageRepository$new(
        root = file.path(Sys.getenv("HOME"), "code/cran_rappster"))
      repo_rappster$register()
      withConditionalWorkingDirectory({
        adaptRversionNumber("data/repo_complete")
        root <- file.path(getwd(), "data/repo_complete")
      })
      
      repo <- PackageRepository$new(root)
      repo$ensure()
      repo$pull(overwrite = TRUE)
    }
    
    withConditionalWorkingDirectory({    
      adaptRversionNumber("data/repo_complete")
      root <- file.path(getwd(), "data/repo_complete")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    file.rename(file.path(tempdir(), basename(root)), file.path(tempdir(), "repo_new"))
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    
    repo_1 <- PackageRepository$new(root = file.path(tempdir(), "repo_complete"))
    repo_2 <- PackageRepository$new(root = file.path(tempdir(), "repo_new"))
  
    #   repo_2$showRegistered()  
    repo_2$unregister(reset = TRUE)
    repo_2$removeHttpRepos()
    repo_1$register()
    repo_2$register()
# print(repo_2$showRegistered()) 
    expect_message(expect_true(repo_2$pull()), "All dependencies exist in repo")
    repo_2$unregister(reset = TRUE)
    repo_1$delete(ask = FALSE)
    repo_2$delete(ask = FALSE)
    
  })
  
  test_that("PackageRepository/pull/partial", {
    
    withConditionalWorkingDirectory({
      adaptRversionNumber("data/repo_complete")
      root <- file.path(getwd(), "data/repo_complete")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    withConditionalWorkingDirectory({
      adaptRversionNumber("data/repo_partial")
      root <- file.path(getwd(), "data/repo_partial")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    
    repo_1 <- PackageRepository$new(root = file.path(tempdir(), "repo_complete"))
    repo_2 <- PackageRepository$new(root = file.path(tempdir(), "repo_partial"))
    
    repo_2$unregister(reset = TRUE)
    repo_2$removeHttpRepos()
    repo_1$register()
    repo_2$register()
    #   repo_2$showRegistered()
    
    expect_false(all(c("bitops", "XML") %in% repo_2$show()$Package))  
    expect_true(repo_2$pull())
    expect_true(all(c("bitops", "XML") %in% repo_2$show()$Package))  
      
    ## Clean up //
    repo_2$unregister(reset = TRUE)
    repo_1$delete(ask = FALSE)
    repo_2$delete(ask = FALSE)
    
  })

  test_that("PackageRepository/pull/other packages", {
    
    withConditionalWorkingDirectory({
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    pkg <- "pkgKitten"
    expect_false(repo$has(pkg))
    expect_true(repo$pull(pkg = pkg))
    expect_true(length(list.files(repo$source, pattern = pkg)) != 0)
    expect_true(length(list.files(repo[[.Platform$pkgType]], 
      pattern = pkg)) != 0)
    expect_true(pkg %in% repo$show(type = "source")$Package)
    expect_true(pkg %in% repo$show(type = .Platform$pkgType)$Package)
  
    ## Clean up //
    repo$delete(ask = FALSE)
    
  })
  
  test_that("PackageRepository/pull/other packages/multiple", {
    
    withConditionalWorkingDirectory({
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    pkg <- c("stringr", "testthat")
    expect_false(all(repo$has(pkg)))
    expect_true(repo$pull(pkg = pkg))
    expect_true(length(list.files(repo$source, pattern = pkg[1])) != 0)
    expect_true(length(list.files(repo$source, pattern = pkg[2])) != 0)
    expect_true(length(list.files(repo[[.Platform$pkgType]], 
      pattern = pkg[1])) != 0)
    expect_true(length(list.files(repo[[.Platform$pkgType]], 
      pattern = pkg[2])) != 0)
    expect_true(pkg[1] %in% repo$show(type = "source")$Package)
    expect_true(pkg[2] %in% repo$show(type = "source")$Package)
    expect_true(pkg[1] %in% repo$show(type = .Platform$pkgType)$Package)
    expect_true(pkg[2] %in% repo$show(type = .Platform$pkgType)$Package)
    
    ## Clean up //
    repo$delete(ask = FALSE)
    
  })
  
  test_that("PackageRepository/pull/other packages/explicit repo", {
    
    withConditionalWorkingDirectory({    
      adaptRversionNumber("data/cran_4")
      root <- file.path(getwd(), "data/cran_4")
    })
    file.copy(root, tempdir(), recursive = TRUE, overwrite = TRUE)
    repo <- PackageRepository$new(root = file.path(tempdir(), "cran_4"))
    
    pkg <- "pkgKitten"
    expect_false(repo$has(pkg))
  #   expect_message(
  #     expect_true(repo$pull(pkg = pkg, repos = "http://mirrors.softliste.de/cran/")),
  #     "mirrors\\.softlist\\.de"
  #   )
    expect_true(repo$pull(pkg = pkg, repos = "http://mirrors.softliste.de/cran/"))
## TODO: implement repo check (e.g. via `capture.output()`)  
    expect_true(length(list.files(repo$source, pattern = pkg)) != 0)
    expect_true(length(list.files(repo[[.Platform$pkgType]], 
      pattern = pkg)) != 0)
    expect_true(pkg %in% repo$show(type = "source")$Package)
    expect_true(pkg %in% repo$show(type = .Platform$pkgType)$Package)
    
    ## Clean up //
    repo$delete(ask = FALSE)
    
  })
}

##------------------------------------------------------------------------------
context("PackageRepository/refresh")
##------------------------------------------------------------------------------

test_that("PackageRepository/refresh", {
  
  root <- file.path(tempdir(), "cran")
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
  expect_true(length(getOption("reposr")$repos_0) > 0)
  
  repo <- PackageRepository$new(root = file.path(tempdir(), "cran"))
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
    adaptRversionNumber("data/cran_2")
    file.copy("data/cran_2", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "cran_2")
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
    adaptRversionNumber("data/cran_2")
    file.copy("data/cran_2", tempdir(), recursive = TRUE)
  })
  root <- file.path(tempdir(), "cran_2")
  repo <- PackageRepository$new(root = root) 
  expect_true(repo$remove(pkg = "dummy", 
    type = "source", ask = FALSE))
  expect_true(length(
    list.files(repo$root, recursive = TRUE, pattern = "dummy")) > 0)
  expect_true(repo$remove(pkg = "dummy", 
    type = .Platform$pkgType, ask = FALSE))
  expect_false(length(
    list.files(repo$root, recursive = TRUE, pattern = "dummy")) > 0)
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/reset")
##------------------------------------------------------------------------------

test_that("PackageRepository/reset", {
  
  root <- file.path(tempdir(), "cran")
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE)
  })
  repo <- PackageRepository$new(root = root)
  expect_true(repo$reset(ask = FALSE))
  expect_true(!length(list.files(repo$root, recursive = TRUE, pattern = "dummy")))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/show")
##------------------------------------------------------------------------------

test_that("PackageRepository/show", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_2")
    root <- file.path(getwd(), "data/cran_2")
  })
  repo <- PackageRepository$new(root = root)
  expect_true(length(index <- repo$show()) > 0)
  expect_equal(index$Package, "dummy")    
  
})

test_that("PackageRepository/show/file", {
  
  withConditionalWorkingDirectory(
    root <- file.path("file://", getwd(), "data/cran_2")
  )
  repo <- PackageRepository$new(root = root)
  expect_true(length(index <- repo$show()) > 0)
  expect_equal(index$Package, "dummy")    
  
})

test_that("PackageRepository/show/http", {
  
  root <- "http://cran.rstudio.com"
  repo <- PackageRepository$new(root = root)
  expect_true(length(index <- repo$show()) > 0)
  expect_true("devtools" %in% index$Package)    
  
})

##------------------------------------------------------------------------------
context("PackageRepository/showRegistered")
##------------------------------------------------------------------------------

test_that("PackageRepository/showRegistered", {
  
  repo <- PackageRepository$new()
  repo$unregister(reset = TRUE)
  if (batch_test) {
    repos <- getOption("repos")
    repos["CRAN"] <- "http://cran.rstudio.com"
    options(repos = repos)
  }
  repo$register()
  expect_equivalent(res <- repo$showRegistered(),
    c(CRAN = file.path("file://", getwd(), "cran"),
      CRAN = "http://cran.rstudio.com",
      CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"
    )
  )  
  target <- if (batch_test) {
    c(CRAN = file.path("file://", getwd(), "cran"),
      CRAN = "http://cran.rstudio.com")
  } else {
    c(CRAN = file.path("file://", getwd(), "cran"))
  }
  expect_identical(res <- repo$showRegistered(custom_only = TRUE), target)
  repo$unregister()
  expect_equivalent(res <- repo$showRegistered(),
    c(CRAN = "http://cran.rstudio.com",
      CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"
    )
  )
  target <- if (batch_test) {
    c(CRAN = "http://cran.rstudio.com")
  } else {
    structure(character(), names = character())
  }
  expect_identical(res <- repo$showRegistered(custom_only = TRUE), target)
  repo$unregister(reset = TRUE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/unregister")
##------------------------------------------------------------------------------

test_that("PackageRepository/unregister", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_2")
    root <- file.path(getwd(), "data/cran_2")
  })
  repo <- PackageRepository$new(root)
  if (batch_test) {
    repos <- getOption("repos")
    repos["CRAN"] <- "http://cran.rstudio.com"
    options(repos = repos)
  }
  repo$register()
  expect_true(length(getOption("repos")) == 3)
  expect_true(repo$unregister())
  expect_true(length(getOption("repos")) == 2)
  
})

test_that("PackageRepository/unregister/reset", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_2")
    root <- file.path(getwd(), "data/cran_2")
  })
  repo_1 <- PackageRepository$new(root)
  repo_1$register()  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_3")
    root <- file.path(getwd(), "data/cran_3")
  })
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
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran_3")
    root <- file.path(getwd(), "data/cran_3")
  })
  repo <- PackageRepository$new(root)
  if (batch_test) {
    repos <- getOption("repos")
    repos["CRAN"] <- "http://cran.rstudio.com"
    options(repos = repos)
  }
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
context("PackageRepository/private/archivePackages")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/archivePackages", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    tmp <- file.path(getwd(), "data/cran")
  })
  file.copy(tmp, tempdir(), recursive = TRUE)
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  # self = repo  
  private <- environment(repo$ensure)$private
  expect_is(res <- private$archivePackages(refresh = FALSE), "list")
  expect_identical(res[[.Platform$pkgType]], c(dummy_1.0 = TRUE, dummy_1.1 = TRUE))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/derive root")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/derive root", {
  
  repo <- PackageRepository$new(root = file.path(tempdir(), "cran"))
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
context("PackageRepository/private/detect repo type")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/detect repo type", {
  
  root <- normalizePath(file.path(tempdir(), "cran"), 
    winslash = "/", mustWork = FALSE)
  root_at <- paste0(root, "_atomic")
  
  repo <- PackageRepository$new(root)
  repo$ensure()
  repo$ensure(atomic = TRUE)
  
  self=repo
  private <- environment(repo$ensure)$private
  expect_identical(private$detectRepoType(), "standard")
  
#   repo <- PackageRepository$new(root, atomic = TRUE)
#   repo$ensure()
#   private <- environment(repo$ensure)$private
#   expect_identical(private$detectRepoType(), "standard")
#   
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
  
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  repo$delete(ask = FALSE)
  private <- environment(repo$ensure)$private
  expect_error(private$ensureIndexFiles())
  repo$ensure()
  expect_true(all(private$ensureIndexFiles()))
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/getLatestPackages")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/getLatestPackages", {
  
  root <- file.path(tempdir(), "cran")
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE)
  })
  repo <- PackageRepository$new(root = root)
  priv <- environment(repo$ensure)$private
  expect_is(res <- priv$getLatestPackages(refresh = FALSE), "list")
  expect_identical(res[[.Platform$pkgType]],
    data.frame(
     name = "dummy",
     version = "1.2",
     type = .Platform$pkgType,
     pattern = "/dummy_1.2\\.",
     fname = "dummy_1.2",
     fpath = file.path(repo[[.Platform$pkgType]], "dummy_1.2.zip"),
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
  
  root <- file.path(tempdir(), "cran")
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    file.copy("data/cran", tempdir(), recursive = TRUE)
  })
  repo <- PackageRepository$new(root = root)
  priv <- environment(repo$ensure)$private
  expect_is(res <- priv$getOldPackages(refresh = FALSE), "list")
  expect_identical(res[[.Platform$pkgType]][1,],
    data.frame(
     name = "dummy",
     version = "1.0",
     type = .Platform$pkgType,
     fpath = file.path(repo[[.Platform$pkgType]], "dummy_1.0.zip"),
     stringsAsFactors = FALSE
    )
  )
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/getVersionMatrixFromDescription")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/getVersionMatrixFromDescription", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    tmp <- file.path(getwd(), "data/cran")
  })
  file.copy(tmp, tempdir(), recursive = TRUE)
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root = root)
  private <- environment(repo$ensure)$private
  expect_is(res <- private$getVersionMatrixFromDescription(), "data.frame")
  expect_true(all(colnames(res) == c("name", "operator", "version")))
  expect_true(nrow(res) > 0)
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/getVersionMatrixFromRepo")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/getVersionMatrixFromRepo", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    tmp <- file.path(getwd(), "data/cran")
  })
  file.copy(tmp, tempdir(), recursive = TRUE)
  root <- file.path(tempdir(), "cran")
  repo <- PackageRepository$new(root)
  private <- environment(repo$ensure)$private
  expect_is(res <- private$getVersionMatrixFromRepo(), "data.frame")
  expect_true(all(colnames(res) == c("name", "version")))
  expect_true(nrow(res) > 0)
  repo$delete(ask = FALSE)
  
})

##------------------------------------------------------------------------------
context("PackageRepository/private/parse index file")
##------------------------------------------------------------------------------

test_that("PackageRepository/private/parse index file", {
  
  withConditionalWorkingDirectory({
    adaptRversionNumber("data/cran")
    root <- file.path(getwd(), "data/cran")
  })
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