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
#' @field type \code{\link{character}}.
#'  Repository path type.
#' @example inst/examples/PackageRepository.r
#' @template author
#' @template references
#' @import conditionr
#' @import miniCRAN
#' @import R6
#' @export
PackageRepository <- R6Class(
  classname = "PackageRepository",
  portable = TRUE,
  
  ##----------------------------------------------------------------------------
  ## Public //
  ##----------------------------------------------------------------------------
  
  public = list(
    type = "character",
    normalize = "logical",
    initialize = function(
      root = "lcran",
      type = c("fs", "url_fs", "url_http", "url_ftp"),
      normalize = TRUE
    ) {
      type <- match.arg(type, c("fs", "url_fs", "url_http", "url_ftp"))
      self$type <- type
      private$subdirs <- c("mac.binary", "source", "win.binary")
      private$.root <- root
      self$normalize <- normalize
    },
    asUrl = function(
      scheme = c("file", "http", "ftp"),
      archive = FALSE
    ) {
      this <- if (!archive) self else PackageRepository$new(self$root_archive)
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
    browse = function(
      type = c("", private$subdirs),
      strict = private$strict,
      archive = FALSE
    ) {
      self_this <- if (!archive) self else PackageRepository$new(self$root_archive)
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
      archive = FALSE,
      ask = TRUE,
      strict = 0,
      plain = FALSE
    ) {
      this <- if (!archive) self else PackageRepository$new(self$root_archive)
      path <- this$root
      if (!file.exists(path)) {
        if (strict == 1) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = path
            ),
            type = "message"
          )
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = path
            ),
            type = "warning"
          )
        } else if (strict == 3) {
          conditionr::signalCondition(
            condition = "NegativeExistenceCheck",
            msg = c(
              "Repository does not exist",
              Path = path
            ),
            type = "error"
          )
        } 
        return(
          if (plain) {
            FALSE
          } else {
            structure(FALSE, names = path)
          }
        )
      }
      if (ask) {
        conditionr::signalCondition(
          condition = "VerifyDeleteOperation",
          msg = c(
            "Verify repository deletion",
            Path = path
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
            structure(FALSE, names = path)
          })
        }
      }
      unlink(path, recursive = TRUE, force = TRUE)
      if (plain) {
        TRUE
      } else {
        structure(TRUE, names = path)
      }
    },
    #' @import miniCRAN
    dependsOn = function(
      pkg = private$getPackageName(),
      type = getOption("pkgType"),
      local_only = FALSE,
      depends = TRUE,
      suggests = TRUE,
      enhances = FALSE,
      register = FALSE,
      ...
    ) {
      if (!self$has(pkg)) {
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
      repo <- if (!local_only) {
        getOption("repos")
      } else {
        self$asUrl()
      }
      if (register) {
        self$register()
      }
      miniCRAN::pkgDep(pkg, repos = repo, 
        type = type, depends = depends, suggests = suggests, 
        enhances = enhances, ...) 
    },
    ensure = function(
      archive = FALSE,
      overwrite = FALSE, 
      ask = TRUE, 
      plain = FALSE
    ) {
      this <- if (!archive) self else PackageRepository$new(self$root_archive)
      path <- this$root
      if (overwrite && file.exists(path)) {
        if (ask) {
          conditionr::signalCondition(
            condition = "VerifyOverwriteOperation",
            msg = c(
              "Verify repository overwrite",
              Path = path
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
              structure(FALSE, names = path)
            })
          }
        }
        self$delete(ask = FALSE, plain = plain)
      } 
      subdirs <- sapply(private$subdirs, function(ii) {
        this[[ii]]
      })
      sapply(subdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
      private$ensureIndexFiles(archive = archive)
      if (plain) {
        TRUE
      } else {
        structure(TRUE, names = path)
      }
    },
    exists = function(
      archive = FALSE, 
      strict = FALSE, 
      plain = FALSE
    ) {
      this <- if (!archive) self else PackageRepository$new(self$root_archive)
      path <- this$root
      if (!file.exists(path)) {
        if (!strict) {
          if (plain) {
            FALSE
          } else {
            structure(FALSE, names = path)
          }
        } else {
          conditionr::signalCondition(
            condition = "InvalidPackageRepositoryLocation",
            msg = c(
              "Package repository directory does not exist",
              Path = path
            ),
            type = "error"
          )
        }
      } else {
        if (plain) {
          TRUE
        } else {
          structure(TRUE, names = path)
        }
      }   
    },
    export = function(
      pkg = character(),
      type = c(getOption("pkgType"), "source"),
      to = tempdir(),
      overwrite = FALSE
    ) {
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
            file.copy(path, repo_to[[ii]], overwrite = overwrite)
            structure(TRUE, names = file.path(repo_to[[ii]], basename(path)))
          }, USE.NAMES = FALSE)
        }, USE.NAMES = FALSE)
        repo_to$refresh()
#         names(out) <- NULL
#         unlist(out)
        out
      }
    },
    visualizeDependencies = function(
      pkg = private$getPackageName(),
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
    },
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
      pkg = private$getPackageName(),
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
      pkg = private$getPackageName(),
      type = c(getOption("pkgType"), "source")
    ) {
      self$ensure()
      self$register()
      deps <- self$dependsOn(pkg)
      sapply(type, function(ii) {
        suppressWarnings(makeRepo(deps, path = self$root, 
          type = ii, download = TRUE))
        pkg_local <- setdiff(deps, self$show(type = ii)$Package)
        if (length(pkg_local)) {
          ## Local CRANs //
          repos_local <- grep("file://", getOption("repos"), value = TRUE)
          pkgs_local <- pkgAvail(repos = repos_local, type = ii)  
          sapply(pkg_local, function(ii_2) {
            tmp <- pkgs_local[which(pkgs_local[, "Package"] %in% ii_2), , drop = FALSE]  
            if (!nrow(tmp)) {
              stop("Local package not found")
            }
            repo_local <- tmp[1, "Repository"]
#             if (!exists(repo_local, .buffer, inherits = FALSE)) {
              repo_local <- PackageRepository$new(
              private$deriveRoot(repo_local, type = ii))  
#               assign()
#             }
            repo_local$export(pkg = ii_2, type = ii, to = self$root, overwrite = TRUE)
          })
        }
      })
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
      if (!length(private$.roption_repo_cache)) {
        private$.roption_repo_cache <- current
      }
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
      pkg = private$getPackageName(),
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
    unregister = function(
      reset = FALSE
    ) {
      repo <- self$asUrl()
      if (reset) {
        if (length(private$.roption_repo_cache)) {
          options("repos" = private$.roption_repo_cache)
          private$.roption_repo_cache <- character()
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
    }
  ),

  ##----------------------------------------------------------------------------
  ## Private //
  ##----------------------------------------------------------------------------

  private = list(
    .root = character(),
    .mac.binary = "bin/macosx/contrib",
    .win.binary = "bin/windows/contrib",
    .source = "src/contrib",
    .roption_repo_cache = character(),
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
              self$root_archive, 
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
    ensureIndexFiles = function(
      archive = FALSE, 
      overwrite = FALSE
    ) {  
      this <- if (!archive) self else PackageRepository$new(self$root_archive)
      path <- this$root
      ## Depends on existinting repository root directory //
      self$exists(archive = archive, strict = TRUE)
      subdirs <- sapply(private$subdirs, function(ii) {
        this[[ii]]
      }, USE.NAMES = FALSE)
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
      ii=2
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
    getPackageName = function() {
      name <- suppressWarnings(try(
        as.list(read.dcf("DESCRIPTION")[1,])$Package, silent = TRUE))
      if (inherits(name, "try-error")) {
        character()
      } else {
        name
      }
    },
    getSubDirs = function(
      value = private$subdirs
    ) {
      value <- match.arg(value, private$subdirs, several.ok = TRUE)
      sapply(value, function(ii) {
        self[[ii]]
      }, USE.NAMES = FALSE)
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
      dcf <- as.data.frame(read.dcf(fpath), stringsAsFactors = FALSE)
      #       dcf <- addClassAttribute(obj = dcf, 
      #         class_name = "RappParsedPackageRepositoryIndexS3")
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

  ##----------------------------------------------------------------------------
  ## Active //
  ##----------------------------------------------------------------------------

  active = list(
    root = function(value) {
      if (!missing(value)) {
        private$.root <- value  
      } 
      value <- private$.root
      type <- self$type
      normalize <- self$normalize
      repos_raw <- gsub("///", "", gsub("^.*(?=///)", "", value, perl = TRUE))
      if (normalize) {
        repos_raw <- normalizePath(repos_raw, winslash = "/", mustWork = FALSE)
      }
      out <- if (type == "fs") {
        repos_raw
      } else if (type == "url_fs") {
        paste0("file:///", repos_raw)
      } else if (type == "url_http"){
        paste0("http://", repos_raw)
      } else if (type == "url_ftp"){
        paste0("ftp://", repos_raw)
      } else {
        conditionr::signalCondition(
          call = quote(self$root),
          condition = "InvalidRepositoryPathType",
          msg = c(
            "Invalid type",
            Type = type,
            Valid = paste(c("fs", "url_fs", "url_http", "url_ftp"),
                             collapse = ", ")
          ),
          type = "error"
        )
      }
      out
    },
    root_archive = function() {
      paste0(self$root, "_archive")
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

