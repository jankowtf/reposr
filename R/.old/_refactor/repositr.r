#' @title
#' Local Package Repository Management
#'
#' @description
#' Functionality for managing local package repositories.
#' The primary goal behind the development of this package was a systematic
#' way for managing own packages in local package repositories in order to 
#' seamlessly integrate them with packrat
#' 
#' @details 
#' The core top-level functions/methods of this package: 
#'  \itemize{
#'    \item{\code{\link[repor]{ensureRepository}}: }{
#'    
#'      Ensures the existence of a local repositories in a directory provided
#'      by argument \code{repos}. 
#'    }
#'    \item{\code{\link[repor]{ensureRepositoryInfrastructure}}: }{
#'    
#'      Ensures a local repository infrastructure below \code{repos_root} 
#'      containing a \emph{global} repository (directory \code{/global}) and 
#'      a \emph{package-and-version-specific } repository for the package to 
#'      be build (directory \code{/packages/{pkg-name}/{pkg-version}}).
#'    }
#'    \item{\code{\link[repor]{buildIntoRepositoryInfrastructure}}: }{
#'      
#'      Builds a package and ensures that it is available in the local repository
#'      infrastructure as created by 
#'      \code{\link[repor]{ensureRepositoryInfrastructure}}.
#'    }
#' }
#' 
#' Other useful top-level functions/methods: 
#'  \itemize{
#'    \item{\code{\link[repor]{parseRepositoryIndex}}: }{
#'    
#'      Parses the content of a repository's index file (\code{PACKAGES}).
#'    }
#'    \item{\code{\link[repor]{refreshRepositoryIndex}}: }{
#'    
#'      Refreshes a repository's index file (\code{PACKAGES}). 
#'    }
#' }
#' 
#' @template author
#' @template references
#' @docType package
#' @name repor
NULL
