#' @title
#' Class: PackageRepository
#'
#' @description
#' Class representing package repositories.
#' 
#' @section Methods:
#' \code{\link[reposr]{testMethod}}
#'    
#' @field root \code{\link{character}}.
#'  Repository's root path.
#' @field scheme \code{\link{character}}.
#'  Repository URL scheme.
#' @field normalize \code{\link{logical}}.
#'  \code{TRUE}: normalize repository root; 
#'  \code{FALSE}: take repository root "as is" (for relative paths).
#' @field detect_scheme \code{\link{logical}}.
#'  \code{TRUE}: detect scheme from repository root; 
#'  \code{FALSE}: no scheme detection.
#' @field packrat \code{\link{logical}}.
#'  \code{TRUE}: a project-based package repository is created that 
#'  integrates seamlessly with \href{packrat}{https://github.com/rstudio/packrat}; 
#'  \code{FALSE}: no project-based repository is created.
#' @example inst/examples/PackageRepository.r
#' @template author
#' @template references
#' @import conditionr
#' @import digest
#' @import miniCRAN
#' @import R6
#' @export
PackageRepository <- R6Class(
  classname = "PackageRepository",
  portable = TRUE,
  
  ##############################################################################
  ## Public //
  ############################################################################## 

  public = list(
    scheme = "none",
    normalize = TRUE,
    detect_scheme = TRUE,
    packrat = FALSE,
    atomic = FALSE,
    initialize = function(
      root = "cran",
      scheme = c("none", "file", "http", "ftp"),
      normalize = TRUE,
      detect_scheme = TRUE,
      packrat = FALSE,
      atomic = FALSE
    ) {
      scheme <- match.arg(scheme, c("none", "file", "http", "ftp"))
      self$scheme <- scheme
      private$subdirs <- c("mac.binary", "source", "win.binary")
      self$normalize <- normalize
      self$detect_scheme <- detect_scheme
      self$packrat <- packrat
      if (packrat) {
        root <- "packrat/cran"
      }
      private$.root <- root
      self$atomic <- atomic
      private$ensureOptions()
    },
    asUrl = function(
      scheme = c("file", "http", "ftp"),
      atomic = FALSE
    ) {
      this <- if (!atomic) self else PackageRepository$new(self$root_atomic)
      path <- this$root
      scheme <- match.arg(scheme, c("file", "http", "ftp"))
#       has_url_prefix <- length(grep(":///", repos)) > 0
      repos_raw <- gsub("///", "", gsub("^.*(?=///)", "", path, perl = TRUE))
#       repos_raw <- normalizePath(repos_raw, winslash = "/", mustWork = FALSE)
      if (scheme == "file") {
        out <- file.path("file://", repos_raw)
      } else if (scheme == "http"){
        out <- file.path("http://", repos_raw)
      } else if (scheme == "ftp"){
        out <- file.path("ftp://", repos_raw)
      } else {
        out <- repos_raw
      }
      out
    },
    asNonUrl = function(
      archive = FALSE
    ) {
      this <- if (!archive) self else PackageRepository$new(self$root_atomic)
      root <- this$root
      private$.asNonUrl(root)
    },
    atomize = function(
#       pkgs = list(),
      type = private$subdirs,
      symlink = FALSE,
      overwrite = FALSE,
      refresh = FALSE
    ) {
#       if (!length(pkgs)) {
        pkgs <- private$getLatestPackages(type = type, refresh = refresh)  
#       }
      out <- lapply(pkgs, function(ii) {
        if (nrow(ii)) {
          sapply(1:nrow(ii), function(row) {
            pkg <- ii[row,]
            root_target <- file.path(
              self$root_atomic, 
              pkg$name,
              pkg$version
            )
            tmp <- PackageRepository$new(root = root_target)
            path_tgt <- file.path(tmp[[pkg$type]], basename(pkg$fpath))
            if (!symlink) {
              tmp$ensure()
              file.copy(
                pkg$fpath, 
                path_tgt,
                overwrite = overwrite
              )
              tmp$refresh()
            } else {
              tmp$ensure(index = FALSE)
              ## Index files //
              environment(tmp$ensure)$private$ensureIndexFileSymlinks(
                root_src = self$root,
                overwrite = overwrite
              )
              ## Package builds //
              if (getOption("pkgType") == "win.binary") {
                if (file.exists(path_tgt) && overwrite) {
                  unlink(path_tgt, force = TRUE)
                }
                capture.output(shell(sprintf("mklink /H %s %s", 
                  normalizePath(path_tgt, mustWork = FALSE),
                  normalizePath(pkg$fpath, mustWork = FALSE)
                ), intern = TRUE))
              } else {
                stop("Symbolic links not supported for this OS yet")
              }
            }
            structure(TRUE, names = sprintf("%s_%s", pkg$name, pkg$version))
          })
        } else {
          FALSE
        }
      })
      names(out) <- type
      out
    },
    browse = function(
      type = c("", private$subdirs),
      strict = private$strict,
      archive = FALSE
    ) {
      self_this <- if (!archive) self else PackageRepository$new(self$root_atomic)
      private_this <- environment(self_this$ensure)$private
      if (!private_this$validateExistence(strict = strict)) {
        return(FALSE)
      }
      type <- match.arg(type, c("", private_this$subdirs))
      path <- if (type == "") {
        self_this$root
      } else {
        private_this$getSubDirs(value = type)
      }
      if (.Platform['OS.type'] == "windows"){
        shell.exec(normalizePath(path, winslash = "/"))
      } else {
        system(paste(Sys.getenv("R_BROWSER"), path))
      }
      message(paste0("Browsing ", path))
      invisible(TRUE)
    },
    buildInto = function(
      binary = TRUE,
      ensure = FALSE,
      refresh = TRUE,
      clean = FALSE
    ) {
      does_exist <- self$exists()
      if (!does_exist) {
        if (ensure) {
          self$ensure()
        } else {
          self$exists(strict = TRUE)
        }
      }
      pkg_type <- getOption("pkgType")
      if (pkg_type == "source") {
        binary <- FALSE
      }
      
      wd <- getwd()
      .buffer <- new.env()
      
#       repos_list <- self$root
#       out <- sapply(seq(along = repos_list), function(ii) {
#         repos <- repos_list[[ii]]
          repos <- self$root
        
        path_bin <- normalizePath(if (pkg_type != "source") {
           self[[pkg_type]]
        }, winslash = "/")
        path_source <- self$source
        tryCatch({
          ## Build and cache //
          devtools::document(pkg = wd)
          
          ## Binary //
          if (binary && !is.null(path_bin)) {
            tmp <- devtools::build(pkg = wd, path = path_bin, binary = TRUE)
            assign(pkg_type, tmp, envir = .buffer)
          }
# .buffer$win.binary
          ## Source //
          tmp <- devtools::build(pkg = wd, path = path_source)
          .buffer$source <- tmp
          if (refresh) {
            self$refresh()
          }
          if (clean) {
            self$clean(refresh = refresh)
          }
          TRUE
        },
        error = function(cond) {
          message(conditionMessage(cond))
          FALSE
        },
        warning = function(cond) {
          warning(conditionMessage(cond))
          TRUE
        }
      )
    },
    clean = function(
      type = private$subdirs,
      archive = TRUE,
      refresh = TRUE
    ) {
      self$exists(strict = TRUE)
      
      index_old <- private$getOldPackages(refresh = !refresh) 
      
      ## Archive //
      if (archive) {
        private$archivePackages(type = type)
      }
      
      ## Loop over sublevel paths //
      sapply(index_old, function(ii) {
        if (nrow(ii)) {
          sapply(ii$fpath, unlink, recursive = TRUE, force = TRUE)
        }
      })
      structure(TRUE, names = self$root)
    },
    delete = function(
      atomic = FALSE,
      ask = TRUE,
      strict = 0,
      plain = FALSE
    ) {
      if (atomic) {
        if (!exists("root", private$cache, inherits = FALSE)) {
          private$cache$root <- self$root
        }
        self$root <- self$root_atomic
        #         self$atomic <- TRUE
        on.exit({
          self$root <- private$cache$root
          #           self$atomic <- FALSE
        })
      }
      root <- self$root
      scheme <- private$detectScheme(root)
      if (scheme %in% c("none", "file")) {
        root <- private$.asNonUrl(root)
        idx <- file.exists(root)
      } else {
        idx <- private$respondsUrl(root)
      }
      if (!idx) {
        if (strict == 1) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = root
            ),
            type = "message"
          )
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = root
            ),
            type = "warning"
          )
        } else if (strict == 3) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = root
            ),
            type = "error"
          )
        } 
        return(
          if (plain) {
            FALSE
          } else {
            structure(FALSE, names = root)
          }
        )
      }
      if (ask) {
        conditionr::signalCondition(
          condition = "VerifyDeleteOperation",
          msg = c(
            "Verify repository deletion",
            Path = root
          ),
          type = "message"
        )
        user_input <- private$askForUserInput(
          msg = "Delete repository? [y]es | [n]o | [q]uit: ")
        if (is.null(user_input) || !user_input) {
          message("Quitting")
          return(if (plain) {
            FALSE
          } else {
            structure(FALSE, names = root)
          })
        }
      }
      if (scheme %in% c("none", "file")) {
        unlink(root, recursive = TRUE, force = TRUE)
      } else {
        conditionr::signalCondition(
          condition = "DeletionOfRemoteNotSupportedYet",
          msg = c(
            "Deletion of remote repositories not supported yet",
            Root = root,
            Suggestions = "delete manually or via FTP client"
          ),
          type = "error"
        )
      }
      if (plain) {
        TRUE
      } else {
        structure(TRUE, names = root)
      }
    },
    #' @import miniCRAN
    dependsOn = function(
      pkg = private$getFromDescription("Package"),
      type = getOption("pkgType"),
      local_only = FALSE,
      depends = TRUE,
      suggests = TRUE,
      enhances = FALSE,
      register = FALSE,
      strict = 3,
      include_self = FALSE,
      ...
    ) {
      strict <- as.numeric(match.arg(as.character(strict), as.character(0:3)))
      if (!all(self$has(pkg))) {
        if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidPackageOrRepo",
            msg = c(
              "Invalid package(s) or repo",
              Repository = self$root,
              Type = type,
              Packages = paste(pkg, collapse = ", "),
              "Trying to build..."
            ),
            type = "message"
          )
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidPackageOrRepo",
            msg = c(
              "Invalid package(s) or repo",
              Repository = self$root,
              Type = type,
              Packages = paste(pkg, collapse = ", "),
              "Trying to build..."
            ),
            type = "warning"
          )
        } else if (strict == 3) {
          conditionr::signalCondition(
            condition = "InvalidPackageOrRepo",
            msg = c(
              "Invalid package(s) or repo",
              Repository = self$root,
              Type = type,
              Packages = paste(pkg, collapse = ", ")
            ),
            type = "error"
          )
        }
        private$ensurePackageInIndex()
#         repo$buildInto()
      }
      repo <- if (!local_only) {
        getOption("repos")
      } else {
        self$asUrl()
      }
      if (register) {
        self$register()
      }
      deps <- miniCRAN::pkgDep(pkg, repos = repo, 
        type = type, depends = depends, suggests = suggests, 
        enhances = enhances, ...) 
      if (!include_self) {
        deps <- deps[deps != pkg]
      }
      deps
    },
    ensure = function(
      atomic = self$atomic,
      overwrite = FALSE, 
      ask = TRUE, 
      plain = FALSE,
      index = TRUE
    ) {
      if (atomic) {
        if (!exists("root", private$cache, inherits = FALSE)) {
          private$cache$root <- self$root
        }
#         self$root <- self$root_atomic
        self$root <- self$root_atomic_pkg_vsn
#         self$atomic <- TRUE
        on.exit({
          self$root <- private$cache$root
#           self$atomic <- FALSE
        })
      }
      root <- self$root
      if (overwrite && file.exists(root)) {
        if (ask) {
          conditionr::signalCondition(
            condition = "VerifyOverwriteOperation",
            msg = c(
              "Verify repository overwrite",
              Root = root
            ),
            type = "message"
          )
          user_input <- private$askForUserInput(
            msg = "Overwrite existing repository? [y]es | [n]o | [q]uit: ")
          if (is.null(user_input) || !user_input) {
            message("Quitting")
            return(if (plain) {
              FALSE 
            } else {
              structure(FALSE, names = root)
            })
          }
        }
        self$delete(ask = FALSE, plain = plain)
      } 
      scheme <- private$detectScheme(root)
      
      if (scheme %in% c("none", "file")) {
      ## Local directory //        
        if (!atomic) {
          subdirs <- sapply(private$subdirs, function(ii) {
            self[[ii]]
          })
          if (scheme == "file") {
            subdirs <- private$.asNonUrl(subdirs)
          }
          sapply(subdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
        } else {
#           repo_tmp <- PackageRepository$new(private$getAtomicRepoPath())$ensure()
          repo_tmp <- PackageRepository$new(self$root)$ensure()
        }
      } else {
      ## Remote directory //
        conditionr::signalCondition(
          condition = "EnsuranceOfRemoteNotSupportedYet",
          msg = c(
            "Ensurance of remote repositories not supported yet",
            Root = root,
            Suggestions = "ensure manually or via FTP client"
          ),
          type = "error"
        )
      }
      if (index) {
        private$ensureIndexFiles()
      }
      if (plain) {
        TRUE
      } else {
        structure(TRUE, names = root)
      }
    },
    exists = function(
      atomic = self$atomic, 
      strict = FALSE, 
      plain = FALSE
    ) {
      if (atomic) {
        if (!exists("root", private$cache, inherits = FALSE)) {
          private$cache$root <- self$root
        }
        self$root <- self$root_atomic_pkg_vsn
        #         self$atomic <- TRUE
        on.exit({
          self$root <- private$cache$root
          #           self$atomic <- FALSE
        })
      }
      root <- self$root
      scheme <- private$detectScheme(root)
      if (scheme %in%  c("none", "file")) {
        if (scheme == "file") {
          root <- private$.asNonUrl(root)
        } 
        idx <- file.exists(root)
      } else {
        idx <- private$respondsUrl(root)
      }
      if (!idx) {
        if (!strict) {
          if (plain) {
            FALSE
          } else {
            structure(FALSE, names = root)
          }
        } else {
          conditionr::signalCondition(
            condition = "InvalidPackageRepositoryLocation",
            msg = c(
              "Package repository directory does not exist",
              Root = root
            ),
            type = "error"
          )
        }
      } else {
        if (plain) {
          TRUE
        } else {
          structure(TRUE, names = root)
        }
      }
    },
    push = function(
      pkg = character(),
      type = c(getOption("pkgType"), "source"),
      to = tempdir(),
      overwrite = FALSE,
      update = FALSE
    ) {
## TODO: implement update functionality (# 10)     
      if (!length(pkg)) {
      ## Entire repo //  
        dir.create(to, recursive = TRUE, showWarnings = FALSE)
        sapply(list.files(self$root, full.names = TRUE), file.copy, to = to, 
          recursive = TRUE, overwrite = overwrite)
        structure(TRUE, names = to)
      } else {
        repo_to <- PackageRepository$new(to)
        repo_to$ensure()
        out <- sapply(type, function(ii) {
          out <- sapply(pkg, function(ii_2) {
            path <- list.files(self[[ii]], pattern = ii_2, full.names = TRUE)
            path <- path[length(path)]
            if (length(path)) {
              file.copy(path, repo_to[[ii]], overwrite = overwrite)
              structure(TRUE, names = file.path(repo_to[[ii]], basename(path)))
            } else {
              structure(FALSE, names = file.path(repo_to[[ii]], ii_2))
            }
          }, USE.NAMES = FALSE)
        }, USE.NAMES = FALSE)
        repo_to$refresh()
#         names(out) <- NULL
#         unlist(out)
        out
      }
    },
    getMissingDependencies = function(
      deps = self$dependsOn(strict = 0),
      type = private$subdirs,
      refresh = TRUE
    ) {
      self$exists(strict = TRUE)
      type <- match.arg(type, private$subdirs, several.ok = TRUE)
      if (refresh) {
        self$refresh()     
      }
      subdirs <- private$getSubDirs(type)
      
      ## Instantiate remaining repos //
#       repos_rem_names <- private$instantiateRemainingRepos()
#       opts <- getOption("reposr")
      
      ## Loop over subdirs //
      out <- lapply(seq(along = subdirs), function(ii) {
        type <- names(subdirs[ii])
        
#         message("Checking for missing dependencies ...")
        
        pkgs <- self$show(type = type)[ , "Package"] 
        if (length(pkgs)) {
#           pkgs_src <- unique(unlist(lapply(repos_rem_names, function(name) {
#             pkgs <- opts[[name]]$show(type = type)[ , "Package"]
#           })))
          setdiff(deps, pkgs)
        } else {
          deps 
        }
      })
      structure(out, names = type)
    },
    
    ## H ##
    
    hasAny = function(
      atomic = FALSE,
      refresh = TRUE
    ) {
      self$exists(strict = TRUE)
      if (refresh) {
        self$refresh()
      }
      subdirs <- private$getSubDirs()
      out <- sapply(seq(along = subdirs), function(ii) {
        type <- names(subdirs[ii])
        path <- subdirs[[ii]]
        structure(length(private$parseIndexFile(type = type)) != 0,
                  names = type)
      })
      if (!atomic) {
        out <- structure(any(out), names = self$root)
      }
      out
    },
    has = function(
      pkg = private$getFromDescription("Package"),
      type = getOption("pkgType"),
      atomic = TRUE
    ) {
      if (!length(pkg)) {
        conditionr::signalCondition(
          condition = "NoPackageNames",
          msg = c(
            "No package names provided",
            Root = self$root,
            Troubleshooting = "make sure that your working directory points to a valid package project"
          ),
          type = "error"
        )
      }
      self$exists(strict = TRUE)
      subdirs <- private$getSubDirs(type)
      .buffer <- new.env()
      .buffer$res_vec <- structure(rep(FALSE, length(pkg)), names = pkg)
      out <- lapply(names(subdirs), function(ii) {
        type <- names(subdirs[ii])
        path <- subdirs[[ii]]
        pkg_index <- private$parseIndexFile(type = type)
        idx <- structure(pkg %in% pkg_index$Package, names = pkg)
        .buffer$res_vec[which(idx)] <- TRUE
        idx
      })
      names(out) <- type
      if (atomic) {
        out <- .buffer$res_vec
      }
      out
    },
    pull = function(
      pkg = character(),
      repos = getOption("repos"),
      type = c("source", getOption("pkgType")),
      atomize = FALSE,
      symlink = FALSE,
      overwrite = FALSE
    ) {
      pkg_self <- private$getFromDescription("Package")
      is_self <- if (!length(pkg)) {
        pkg <- pkg_self
        TRUE
      } else {
        FALSE
      }
      
      self$ensure()
      self$register()
      on.exit(self$unregister())
      
      deps <- self$dependsOn(pkg, include_self = !is_self, strict = 0)
      
      ## Internal/additional/non-CRAN packages //
      private$ensureAdditionalRepositories(deps = deps)
## TODO: refactor/make more intuitive/informative (# 11)      

      pullInner = function(deps, repos, type, atomize, symlink) {
#         conditionr::signalCondition(
#           condition = "PackageDependencyPull",
#           msg = c(
#             "Pulling package dependencies",
#             Type = type,
#             Dependencies = paste(deps, collapse = ", ")
#           ),
#           type = "message"
#         )
        suppressWarnings(miniCRAN::makeRepo(deps, path = self$root, 
          repos = repos, type = type, download = TRUE))
        private$pullFromFileSystemRepos(deps = deps, type = type)
        if (atomize) {
          self$atomize(type = type, symlink = symlink)                       
        }
      }
      
      if (overwrite) {
        sapply(type, function(ii) {
          pullInner(deps = deps, repos = repos, type = ii, 
            atomize = atomize, symlink = symlink)
        })
      } else {
        sapply(type, function(ii) {
          deps <- self$getMissingDependencies(deps, type = ii, 
            refresh = FALSE)[[1]]        
          if (length(deps)) {
            pullInner(deps = deps, repos = repos, type = ii, 
              atomize = atomize, symlink = symlink)
          } else {
            message("All dependencies exist in repo")
          }
        })
## TODO: include version information in decision whether to pull a certain
## package or not (# 12)       
      }
      self$refresh()
      TRUE
    },
    refresh = function() {
      structure(all(private$ensureIndexFiles(overwrite = TRUE)), 
        names = self$root)
    },
    register = function(
      name = "CRAN",
      before_cran = TRUE
    ) {
      repo <- self$asUrl()
      current <- getOption("repos")
#       if (!length(private$.roption_repo_cache)) {
#         private$.roption_repo_cache <- current
#       }
      if (!repo %in% current) {
        updated <- if (before_cran) {
          c(structure(repo, names = name), current)
        } else {
          c(current, structure(repo, names = name))
        }
        options("repos" = updated)
      }
      TRUE
    },
    remove = function(
      pkg = private$getFromDescription("Package"),
      type = private$subdirs,
      ask = TRUE,
      clean = FALSE
    ) {
      ## Early exit //
      if (!length(pkg)) {
        conditionr::signalCondition(
          condition = "NoPackageNames",
          msg = c(
            "No package names provided",
            Root = self$root,
            Troubleshooting = "make sure that your working directory points to a valid package project"
          ),
          type = "error"
        )
      }
      if (clean) {
        self$clean()
      } else {
        self$exists(strict = TRUE)
      }
      type <- match.arg(type, private$subdirs, several.ok = TRUE) 
      subdirs <- private$getSubDirs(type)
      .buffer <- new.env()
      .buffer$res_vec <- structure(rep(FALSE, length(pkg)), names = pkg)
      out <- lapply(type, function(ii) {
        pkg_index <- private$parseIndexFile(type = ii)
        idx <- unlist(lapply(paste0("^", pkg, "$"), grep, pkg_index$Package))
        if (length(idx)) {
          .buffer$res_vec[pkg_index$Package[idx]] <- TRUE
          pattern <- paste0(
            pkg_index$Package[idx], "_", pkg_index$Version[idx], "\\.")
          files <- list.files(subdirs[ii], recursive = TRUE, 
            pattern = pattern, full.names = TRUE)
          if (length(files)) {
            if (ask) {
              conditionr::signalCondition(
                condition = "VerifyRemoveOperation",
                msg = c(
                  "Verify package removal",
                  "Repo subtype" = ii,
                  Packages = paste(pkg_index$Package[idx], collapse = ", ")
                ),
                type = "message"
              )
              user_input <- private$askForUserInput(
                msg = "Remove those packages? [y]es | [n]o | [q]uit: ")
              if (is.null(user_input) || !user_input) {
                message("Quitting")
                return(NULL)
              }
            }
            sapply(files, unlink, force = TRUE)
            self$refresh()
          }
        }
      })
      .buffer$res_vec
    },
    removeHttpRepos = function(
      id = character()
    ) {
      repos <- getOption("repos")  
      pattern <- if (length(id)) sprintf("^http://.*%.*", id) else "^http://.*"
      idx <- grepl(pattern, repos)
      repos <- repos[!idx]
      options("repos" = repos)
      TRUE
    },
    reset = function(
      ask = TRUE,
      type = c("mac.binary", "source", "win.binary")
    ) {
      type <- match.arg(type, c("mac.binary", "source", "win.binary"),
                        several.ok = TRUE)
      subdirs <- sapply(private$subdirs, function(ii) {
        self[[ii]]
      }, USE.NAMES = FALSE)[type]
      if (all(names(subdirs) %in% c("mac.binary", "source", "win.binary"))) {
        subdirs <- self$root
      }
      if (ask) {
        conditionr::signalCondition(
          condition = "VerifyResetOperation",
          msg = c(
            "Verify repository reset",
            Paths = paste(subdirs, collapse = ", ")
          ),
          type = "message"
        )
        user_input <- private$askForUserInput(
          msg = "Reset repository? [y]es | [n]o | [q]uit: ")
        if (is.null(user_input) || !user_input) {
          message("Quitting")
          return(FALSE)
        }
      }
      
      sapply(subdirs, function(ii) {
        unlink(ii, recursive = TRUE, force = TRUE)
      })
      self$ensure()
    },
    show = function(
      type = getOption("pkgType")
    ) {
      private$parseIndexFile(type = type)
    },
    showRegistered = function(
      custom_only = FALSE
    ) {
      out <- getOption("repos")
      if (custom_only) {
        standard <- getOption("reposr")$repos_0
        out <- out[!out %in% standard]
      }
      out
    },
    unregister = function(
      reset = FALSE
    ) {
      repo <- self$asUrl()
      if (reset) {
#         if (length(private$.roption_repo_cache)) {
        if (length(repos_0 <- getOption("reposr")$repos_0)) {          
#           options("repos" = private$.roption_repo_cache)
#           private$.roption_repo_cache <- character()
          options("repos" = repos_0)
          TRUE
        } else {
          FALSE
        }
      } else {
        current <- getOption("repos")
        if (length(idx <- which(current %in% repo))) {
          updated <- current[-idx]
          options("repos" = updated)
          TRUE
        } else {
          FALSE
        }
      }
    },
    visualizeDependencies = function(
      pkg = private$getFromDescription("Package"),
      export = character(),
      ...
    ) {
      p <- makeDepGraph(pkg, enhances = TRUE)
      if (!length(export)) {
        plot(p)
        invisible(NULL)
      } else {
        if (grepl("\\.svg$", export)) {
          svg(export, ...)  
        } else if (grepl("\\.png$", export)) {
          png(export, ...)
        } else if (grepl("\\.pdf$", export)) {
          pdf(export, ...)
        } else {
          stop("Specify file extension (.svg, .png or .pdf)")
        }
        
        suppressWarnings(plot(p))
        dev.off()
        export
      }
    }
  ),

  ##############################################################################
  ## Private //
  ##############################################################################

  private = list(
    .root = character(),
    .mac.binary = "bin/macosx/contrib",
    .win.binary = "bin/windows/contrib",
    .source = "src/contrib",
#     .roption_repo_cache = character(),
    cache = new.env(parent = emptyenv()),
    rversion = paste(
      R.version$major, 
      unlist(strsplit(R.version$minor, split="\\."))[1], sep="."
    ),
    subdirs = "character",
    strict = 0:3,
    archivePackages = function(
      type = private$subdirs,
      overwrite = FALSE,
      refresh = FALSE
    ) {
      old <- private$getOldPackages(type = type, refresh = refresh)  
      out <- lapply(old, function(ii) {
        if (nrow(ii)) {
          sapply(1:nrow(ii), function(row) {
            old <- ii[row,]
            root <- file.path(
              self$root_atomic, 
              old$name,
              old$version
            )
            tmp <- PackageRepository$new(root = root)
            tmp$ensure()
            file.copy(
              old$fpath, 
              file.path(tmp[[old$type]], basename(old$fpath)),
              overwrite = overwrite
            )
            tmp$refresh()
            structure(TRUE, names = sprintf("%s_%s", old$name, old$version))
          })
        } else {
          FALSE
        }
      })
      names(out) <- type
      out
    },
    askForInternalRepository = function(
      msg = "Please select root directory of internal repository ([c]choose | [t]ype | [q]uit): "
    ) {
      input <- readline(msg)
      if (grepl("[cC]|choose|Choose|CHOOSE", input)) {
        normalizePath(choose.dir(), winslash = "/")
      } else if (grepl("[tT]|type|Type|TYPE", input)) {
        normalizePath(
          readline("Please type repository root path: "), winslash = "/")
      } else if (grepl("[qQ]|quit|Quit|QUIT", input)) {
        NULL 
      }
    },
    askForUserInput = function(
      msg = "Continue? [y]es | [n]o | [q]uit: ", 
      force = logical()
    ) {
      if (!length(force)) {
        input <- readline(msg)
        private$processUserInput(input = input, dflt = "yes")
      } else {
        force
      }
    },
    .asNonUrl = function(value) {
      gsub("///?", "", gsub("^.*/?(?=//)", "", value, perl = TRUE))
    },
    createFakeRepoIndex = function() {
      deps <- private$getDependenciesFromDescription()
      cnt <- list(
        Package = private$getFromDescription("Package"),
        Version = private$getFromDescription("Version"),
        Depends = if (!is.null(deps$Depends)) {
          paste(deps$Depends, collapse = ", ")
        },
        Imports = if (!is.null(deps$Imports)) {
          paste(deps$Imports, collapse = ", ")
        },
        Suggests = if (!is.null(deps$Suggests)) {
          paste(deps$Suggests, collapse = ", ")
        },
        License = private$getFromDescription("License"),
        MD5sum = "6b04ea09ab7d4e628f18075c9b6e93f8", ## fake
        NeedsCompilation = private$getFromDescription("NeedsCompilation")
      )
      cnt[sapply(cnt, is.null)] <- NULL
      cnt
    },
    detectRepoType = function() {
      subdirs <- file.path(self$root, c(private$.source, private$.mac.binary, 
        private$.win.binary))
      files <- list.files(self$root)
      if (!self$exists()) {
        stop("Ensure repository first (`$ensure()`")
      }
      ch_std_1 <- all(c("src", "bin") %in% files)
#       ch_ato_1 <- all(!c("src", "bin") %in% files) 
      if (ch_std_1) {
        dirs <- list.dirs(self$root)
        ch_std_2 <- all(subdirs %in% dirs)
        if (ch_std_2) {
          "standard"
        } else {
          "unknown"
        }
      } else {
        "atomic"
      }
    },
    detectScheme = function(
      input
    ) {
      if (grepl("http://", input)) {
        "http"
      } else if (grepl("ftp://", input)) {
        "ftp"
      } else if (grepl("file://", input)) {
        "file"
      } else {
        "none"
      }
    },
    deriveRoot = function(
      input,
      type = getOption("pkgType")
    ) {
      if (type == "source") {
        gsub(paste0("file:///|/", private$.source, ".*$"), "", input)
      } else if (type == "mac.binary") {
        gsub(paste0("file:///|/", private$.mac.binary, ".*$"), "", input)
      } else if (type == "win.binary") {
        gsub(paste0("file:///|/", private$.win.binary, ".*$"), "", input)
      } 
    },
    ensureAdditionalRepositories = function(deps) {
      pkgs <- available.packages()
      idx <- which(!deps %in% pkgs[, "Package"])
      while(length(idx)) {
        if (interactive()) {
          conditionr::signalCondition(
            condition = "PackagesNotInRepository",
            msg = c(
              "Packages not found in registered repositories",
              Packages = paste(deps[idx], collapse = ", "),
              "Prompting for specification of alternative/internal repository..."
            ),
            type = "message"
          )
        } else {
          conditionr::signalCondition(
            condition = "PackagesNotInRepository",
            msg = c(
              "Packages not found in repository",
              Packages = paste(deps[idx], collapse = ", ")
            ),
            type = "error"
          )
        }
        repo_internal <- PackageRepository$new(
          private$askForInternalRepository())
        repo_internal$register()
        pkgs <- available.packages()
        idx <- which(!deps %in% pkgs[, "Package"])
      }
    },
    ensureIndexFiles = function(
      overwrite = FALSE
    ) {  
      path <- self$root
      ## Depends on existinting repository root directory //
      self$exists(strict = TRUE)
      subdirs <- sapply(private$subdirs, function(ii) {
        self[[ii]]
      }, USE.NAMES = FALSE)
      scheme <- private$detectScheme(self$root)
      if (scheme %in% c("none", "file")) {
        subdirs <- private$.asNonUrl(subdirs)
        out <- sapply(seq(along = subdirs), function(ii) {     
          path <- subdirs[[ii]]
          type <- names(subdirs[ii])
          fpath <- file.path(path, c("PACKAGES", "PACKAGES.gz"))
          out <- if (!all(file.exists(fpath)) | overwrite) {
            wd_0   <- getwd()
            on.exit(setwd(wd_0))
            tryCatch({
              setwd(path)
              #         tools::write_PACKAGES(".", type=.Platform$pkgType)
              tools::write_PACKAGES(".", type = type)         
              TRUE
            },
            error = function(cond) {
              message(conditionMessage(cond))
              FALSE
            },
            warning = function(cond) {
              message(conditionMessage(cond))
              TRUE
            },
            finally = setwd(wd_0)
            )
          } else {
            TRUE
          }
          names(out) <- path
          out
        })
        out
      } else {
        conditionr::signalCondition(
          condition = "EnsuranceOfRemoteNotSupportedYet",
          msg = c(
            "Ensurance of index files in remote repositories not supported yet",
            Root = root,
            Suggestions = "ensure manually or via FTP client"
          ),
          type = "error"
        )
      }
    },
    ensureIndexFileSymlinks = function(
      root_src,
      overwrite = FALSE
    ) {  
      this <- self 
      root <- this$root
      ## Depends on existinting repository root directory //
      self$exists(strict = TRUE)
      subdirs <- sapply(private$subdirs, function(ii) {
        this[[ii]]
      }, USE.NAMES = FALSE)
      scheme <- private$detectScheme(root)
      if (scheme %in% c("none", "file")) {
        subdirs <- private$.asNonUrl(subdirs)
        out <- sapply(seq(along = subdirs), function(ii) {     
          path <- subdirs[[ii]]
          targets <- file.path(path, c("PACKAGES", "PACKAGES.gz"))
          srcs <- gsub(root, root_src, targets)
          sapply(1:length(targets), function(ii_2) {
            path_tgt <- targets[ii_2]
            path_src <- srcs[ii_2]
            if (getOption("pkgType") == "win.binary") {
              if (file.exists(path_tgt) && overwrite) {
                unlink(path_tgt, force = TRUE)
              }
              capture.output(shell(sprintf("mklink /H %s %s", 
                normalizePath(path_tgt, mustWork = FALSE),
                normalizePath(path_src, mustWork = FALSE)
              ), intern = TRUE))
            } else {
              stop("Symbolic links not supported for this OS yet")
            }
          })
          TRUE
        })
      } else {
        conditionr::signalCondition(
          condition = "EnsuranceOfRemoteNotSupportedYet",
          msg = c(
            "Ensurance of index files in remote repositories not supported yet",
            Root = root,
            Suggestions = "ensure manually or via FTP client"
          ),
          type = "error"
        )
      }
      out
    },
    ensureOptions = function() {
      cont <- getOption("reposr")
      if (is.null(cont)) {
        cont <- new.env(parent = emptyenv())
        cont$repos_0 <- getOption("repos")
        options(reposr = cont)
      }
      TRUE
    },
    ensurePackageInIndex = function(
      type = c("source", getOption("pkgType"))
    ) {
      index <- private$createFakeRepoIndex()
      sapply(type, function(ii) {
        if (!self$has(type = ii)) {
          path <- private$getSubDirs(ii)
          write.dcf("", file = file.path(path, "PACKAGES"), append = TRUE)
          write.dcf(index, file = file.path(path, "PACKAGES"), append = TRUE)
        }
      })
    },
    getAtomicRepoPath = function() {
      file.path(self$root, private$getFromDescription("Package"),
        private$getFromDescription("Version"))  
    },
    getDependenciesFromDescription = function(path = "DESCRIPTION") {
      desc <- as.list(read.dcf(path)[1,])
      deps <- desc[c("Depends", "Imports", "Suggests")]
      names(deps) <- c("Depends", "Imports", "Suggests")
      lapply(deps, function(ii) {
        unlist(strsplit(gsub("\\n|\\s?| \\(.*\\)", "", ii), ","))
      })
    },
    getFromDescription = function(field = character(), path = "DESCRIPTION") {
      out <- suppressWarnings(try(
        as.list(read.dcf(path)[1,])[[field]], silent = TRUE))
      if (inherits(out, "try-error")) {
        character()
      } else {
        out
      }
    },
    getLatestPackages = function(
      type = private$subdirs,
      refresh = TRUE
    ) {
      self$exists(strict = TRUE)
      type <- match.arg(type, private$subdirs, several.ok = TRUE)
      if (refresh) {
        self$refresh()
      }
      subdirs <- private$getSubDirs(type)
      
      ## Loop over subdirs //
      out <- lapply(seq(along = subdirs), function(ii) {
        type <- names(subdirs[ii])
        path <- subdirs[[ii]]
        
        files <- list.files(path, full.names = TRUE)
      
        pkgs <- private$parseIndexFile(type = type)     
        
        ## Object //
        out <- if (length(pkgs)) {
          data.frame(
            name = pkgs$Package,
            version = pkgs$Version,
            type = type,
            pattern = paste0("/", pkgs$Package, "_", pkgs$Version, "\\."),
            fname = paste0(pkgs$Package, "_", pkgs$Version),
            fpath = NA_character_,
            index = NA_integer_,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            name = character(),
            version = character(),
            type = character(),
            pattern = character(),
            fname = character(),
            fpath = character(),
            index = integer(),
            stringsAsFactors = FALSE
          )
        }
        
        ## Index //
        for (ii in out$pattern) {
          tmp <- grep(ii, files)
          if (length(tmp)) {
            out[out$pattern == ii, "index"] <- tmp
            out[out$pattern == ii, "fpath"] <- files[tmp]
          }
        }
        out
      })
      structure(out, names = type)
    },
    getOldPackages = function(
      type = private$subdirs,
      refresh = TRUE
    ) {
      self$exists(strict = TRUE)
      type <- match.arg(type, private$subdirs, several.ok = TRUE)
      if (refresh) {
        self$refresh()
      }
      subdirs <- private$getSubDirs(type)

      ## Loop over subdirs //
      out <- lapply(seq(along = subdirs), function(ii) {
        type <- names(subdirs[ii])
        path <- subdirs[[ii]]
        
        files <- list.files(path, full.names = TRUE)
        files <- files[!grepl("PACKAGES", files)]
        idx <- seq(along = files)
        
        pkgs <- private$parseIndexFile(type = type)     
        pattern <- paste0("/", pkgs$Package, "_", pkgs$Version, "\\.")
        pattern_2 <- paste0(pkgs$Package, "_\\d")
        idx_keep <- unlist(lapply(pattern, grep, files))
        out <- data.frame(
          name = character(),
          version = character(),
          type = character(),
          fpath = character(),
          stringsAsFactors = FALSE
        )
        if (length(idx_keep)) {
          idx <- idx[-idx_keep]
          if (length(idx)) {
            files <- files[idx]  
            fnames <- gsub(paste0(".*(?=", pattern_2, ")"), 
              "", files, perl = TRUE)
            ## Pre-allocate //
            dummy <- rep(NA_character_, length(fnames))
            out <- data.frame(
              name = dummy,
              version = dummy,
              type = rep(type, length(fnames)),
              fpath = dummy,
              stringsAsFactors = FALSE
            )
            ## Fill //
            lapply(seq(along = fnames), function(ii) {
              tmp <- unlist(strsplit(fnames[ii], split = "_"))
              tmp[2] <- gsub("(?<=\\d)\\.*$", "", gsub("[[:alpha:]]", 
                "", tmp[2]), perl = TRUE)
              out[ii, "name"] <<- tmp[1]
              out[ii, "version"] <<- tmp[2]
              out[ii, "fpath"] <<- grep(fnames[ii], files, value = TRUE)
              NULL
            })
            out
          }
        }
        out
      })
      structure(out, names = type)
    },
    getRepoUid = function(repo = self$root) {
      repo <- gsub("file:///", "", repo)
      sapply(repo, function(ii) {
        sprintf("repo_%s", digest(unname(ii)))  
      })
    },
    getSubDirs = function(
      value = private$subdirs
    ) {
      value <- match.arg(value, private$subdirs, several.ok = TRUE)
      sapply(value, function(ii) {
        self[[ii]]
      }, USE.NAMES = FALSE)
    },
    getVersionMatrixFromDescription = function(path = "DESCRIPTION") {
      desc <- as.list(read.dcf(path)[1,])
      deps <- unique(c(desc$Depends, desc$Imports, desc$Suggests))
      deps <- unlist(strsplit(gsub("\\n", "", deps), ","))  
      deps <- strsplit(deps, "\\(")
      do.call("rbind", lapply(deps, function(ii) {
        if (length(ii) > 1) {
          tmp <- unlist(strsplit(ii[2], " "))
          data.frame(name = gsub("\\s", "", ii[1]), operator = tmp[1], 
            version = gsub("\\)", "", tmp[2]), stringsAsFactors = FALSE)
        } else {
          data.frame(name = gsub("\\s", "", ii[1]), operator = NA, 
            version = NA, stringsAsFactors = FALSE)
        }
      }))
    },
    getVersionMatrixFromRepo = function(repo = self) {
      if (!length(repo)) {
        repo <- getOption("repos")["CRAN"][1]
      }
#       repo <- reposr::PackageRepository$new(repo) 
      # file.exists(repo$asNonUrl())
      vsn_mat <- repo$show()[ , c("Package", "Version")]
      colnames(vsn_mat) <- c("name", "version")
      vsn_mat
    },
    instantiateRemainingRepos = function() {
      repos <- self$showRegistered() 
      repos <- gsub("file:///", "", repos[repos != self$asUrl()])
      opts <- getOption("reposr")
      out <- sapply(repos, function(ii) {
        tmp <- PackageRepository$new(ii)  
        name <- private$getRepoUid(ii)
        opts[[name]] <- tmp
        name
      })
      names(out) <- repos
      out
    },
    parseIndexFile = function(
      type = getOption("pkgType")
    ) {
      self$exists(strict = TRUE)
      
      type <- match.arg(type, private$subdirs)
      fname <- "PACKAGES"
      fpath <- if (type == "mac.binary") {
        file.path(self$mac.binary, fname)  
      } else if (type == "win.binary") {
        file.path(self$win.binary, fname)  
      } else if (type == "source") {
        file.path(self$source, fname)  
      }
      scheme <- private$detectScheme(self$root)
      if (scheme %in% c("none", "file")) {
        fpath <- private$.asNonUrl(fpath)
      } else {
        fpath <- url(fpath)
      } 
      dcf <- as.data.frame(read.dcf(fpath), stringsAsFactors = FALSE)
      dcf
    },
    processUserInput = function(input, dflt = "yes") {
      input <- ifelse(grepl("\\D", input), tolower(input), dflt)
      if (grepl("[qQ]|Quit|quit|QUIT", input)) {
        NULL
      } else if (grepl("[nN]|No|no|NO", input)) {
        FALSE
      } else if (grepl("[yY]|Yes|yes|YES", input)) {
        TRUE
      } else {
        message(paste0("Invalid input: ", input))
        NULL
      }
    },
    pullFromFileSystemRepos = function(
      deps,
      repos = getOption("repos"),
      type = getOption("pkgType")
    ) {
      pkg_local <- setdiff(deps, self$show(type = type)$Package)
      if (length(pkg_local)) {
        ## Local CRANs //
        repos_local <- grep("file://", repos, value = TRUE)
        pkgs_local <- pkgAvail(repos = repos_local, type = type)  
        sapply(pkg_local, function(ii_2) {
          tmp <- pkgs_local[which(pkgs_local[, "Package"] %in% ii_2), , 
            drop = FALSE]  
          if (!nrow(tmp)) {
            conditionr::signalCondition(
              condition = "InternalPackageNotFound",
              msg = c(
                "Internal package not found",
                Package = ii_2,
                Type = type,
                Repo = self[[type]]
              ),
              type = "error"
            )
          }
          repo_local <- tmp[1, "Repository"]
          #             if (!exists(repo_local, .buffer, inherits = FALSE)) {
          repo_local <- PackageRepository$new(
            private$deriveRoot(repo_local, type = type))  
          #               assign()
          #             }
          conditionr::signalCondition(
            condition = "PullingPackageFromFileSystemRepo",
            msg = c(
              "Pulling package from file system repo",
              Package = ii_2,
              Type = type,
              Repo = unname(repo_local[[type]])
            ),
            type = "message"
          )
          repo_local$push(pkg = ii_2, type = type, to = self$root, 
            overwrite = TRUE)
        })
      }
    },
    respondsUrl = function(x) {
      !inherits(try(readLines(x)), "try-error")
    },
    validateExistence = function(
      strict = 0:3
    ) {
      strict = match.arg(as.character(strict), as.character(0:3))
      if (!file.exists(self$root)) {
        if (strict == 1) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = self$root
            ),
            type = "message"
          )
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = self$root
            ),
            type = "warning"
          )
        } else if (strict == 3) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = self$root
            ),
            type = "error"
          )
        }
        FALSE
      } else {
        TRUE
      }
    }
  ),

  ##############################################################################
  ## Active //
  ##############################################################################

  active = list(
    root = function(value) {
      if (!missing(value)) {
        private$.root <- value  
      } 
      value <- private$.root
      scheme <- self$scheme
      scheme_det <- private$detectScheme(value)
      if (scheme_det != scheme && self$detect_scheme) {
        scheme <- scheme_det
        self$scheme <- scheme_det
      }
      
      normalize <- self$normalize
#       repos_raw <- gsub("///", "", gsub("^.*(?=///)", "", value, perl = TRUE))
      repos_raw <- gsub("///?", "", gsub("^.*/?(?=//)", "", value, perl = TRUE))
      if (normalize && !scheme %in% c("http", "ftp")) {
        repos_raw <- normalizePath(repos_raw, winslash = "/", mustWork = FALSE)
      }
      out <- if (scheme == "none") {
        repos_raw
      } else if (scheme == "file") {
        paste0("file:///", repos_raw)
      } else if (scheme == "http"){
        paste0("http://", repos_raw)
      } else if (scheme == "ftp"){
        paste0("ftp://", repos_raw)
      } else {
        conditionr::signalCondition(
          call = quote(self$root),
          condition = "InvalidRepositoryPathType",
          msg = c(
            "Invalid scheme",
            Scheme = scheme,
            Valid = paste(c("none", "file", "http", "ftp"),
                             collapse = ", ")
          ),
          type = "error"
        )
      }
      out
    },
    root_atomic = function() {
      paste0(self$root, "_atomic")
    },
    root_atomic_pkg = function() {
      sprintf("%s_atomic/%s", self$root, private$getFromDescription("Package"))
    },
    root_atomic_pkg_vsn = function() {
      sprintf("%s/%s", self$root_atomic_pkg, private$getFromDescription("Version"))
    },
    mac.binary = function() {
      structure(file.path(self$root, private$.mac.binary, private$rversion),
                names = "mac.binary")
    },
    win.binary = function() {
      structure(file.path(self$root, private$.win.binary, private$rversion),
                names = "win.binary")
    },
    source = function() {
      structure(file.path(self$root, private$.source), names = "source")
    }
  )
)

