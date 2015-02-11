#' @title
#' Local Package Repository Management
#'
#' @description
#' Manage local CRAN-compliant package repositories. Integrates well 
#' (or will eventually do so) with 
#' \href{https://github.com/hadley/devtools}{devtools}, 
#' \href{https://github.com/rstudio/packrat}{packrat},
#' \href{https://github.com/RevolutionAnalytics/miniCRAN}{miniCRAN},
#' \href{https://github.com/RevolutionAnalytics/checkpoint}{checkpoint} and
#' \href{https://github.com/eddelbuettel/drat}{drat}.
#' 
#' @section Classes:
#' 
#' \itemize{
#'    \item{\code{\link[repor]{PackageRepository}}: }{
#'    
#'      Class representing repository structures
#'    }
#' }
#' 
#' @section Main functions and methods:
#' 
#'  \itemize{
#'    \item{\code{\link[repor]{ensure}}: }{
#'    
#'      Creates/ensure a local package repository with
#'      \code{repo$ensure()}
#'    }
#'    \item{\code{\link[repor]{exists}}: }{
#'    
#'      Verify existence with
#'      \code{repo$exists()}
#'    }
#'    \item{\code{\link[repor]{register}}: }{
#'    
#'      Register your repository in the R options with 
#'      \code{repo$register()}
#'    }
#'    \item{\code{\link[repor]{browse}}: }{
#'    
#'      Browse content with
#'      \code{repo$browse()}, \code{repo$browse(type = "source")},
#'      \code{repo$browse(type = "mac.binary")} and 
#'      \code{repo$browse(type = "win.binary")}
#'    }
#'    \item{\code{\link[bumpr]{buildInto}}: }{
#'    
#'      Build your package directly into your local package repository
#'      with \code{repo$buildInto()}
#'    }
#'    \item{\code{\link[repor]{show}}: }{
#'    
#'      Show index content with
#'      \code{repo$show()}, \code{repo$show(type = "source")},
#'      \code{repo$show(type = "mac.binary")} and 
#'      \code{repo$show(type = "win.binary")}
#'    }
#'    \item{\code{\link[repor]{has}}: }{
#'    
#'      Look for specific packages with
#'      \code{repo$has()}, 
#'      \code{repo$has(type = "source", atomic = FALSE)},
#'      \code{repo$hasPackager(type = "mac.binary")} and 
#'      \code{repo$has(type = "win.binary")}
#'    }
#'    \item{\code{\link[repor]{refresh}}: }{
#'    
#'      Refresh with
#'      \code{repo$refresh()}
#'    }
#'    \item{\code{\link[repor]{clean}}: }{
#'    
#'      Clean (remove/archive outdated packages and refresh) with
#'      \code{repo$clean()}, 
#'      \code{repo$clean(type = "source")},
#'      \code{repo$clean(type = "mac.binary")} and 
#'      \code{repo$clean(type = "win.binary")}
#'    }
#'    \item{\code{\link[repor]{remove}}: }{
#'    
#'      Remove specific packages with
#'      \code{repo$remove()}, 
#'      \code{repo$remove(type = "source")},
#'      \code{repo$remove(type = "mac.binary")} and 
#'      \code{repo$remove(type = "win.binary")}
#'    }
#'    \item{\code{\link[repor]{reset}}: }{
#'    
#'      Reset entire repository with
#'      \code{repo$reset()}
#'    }
#'    \item{\code{\link[repor]{delete}}: }{
#'    
#'      Delete entire repository with
#'      \code{repo$delete()}
#'    }
#' }
#' 
#' @section Disclaimer:
#' 
#' \itemize{
#'    \item{
#'    \strong{This package is really new. So please test it with package builds
#'    that are not crucial for your productive work!}
#'    }
#' }
#' 
#' 
#' @template author
#' @template references
#' @docType package
#' @name repor
#' @seealso \url{https://github.com/rappster/repor}
NULL
