packrat::off()
# arg_string <- paste0("--library=\"", .libPaths()[length(.libPaths())], "\"")
# options("devtools.install.args" = arg_string)
# getOption("devtools.install.args")
repos_home <- "q:/home/repos/r"
ensureRepositoryInfrastructure(repos_home = repos_home)

repos_home_2 <- getRepositoryInfrastructurePaths(repos_home = repos_home)

sapply(repos_home_2, function(ii) {
  devtools::build(path = getRepositoryPathByType(repos = ii, type = "win.binary"),
                  binary = TRUE)
  devtools::build(path = getRepositoryPathByType(repos = ii, type = "source"))
  refreshRepositoryIndex(repos = ii)
  TRUE
})

