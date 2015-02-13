reposr (0.2)
======

Local Package Repository Management

## Installation

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/reposr")
require("reposr")
```

## Typical workflow 

### Specifiy root directory 

We will use the following directory as our local CRAN-like package repoitory:

```
root <- file.path(tempdir(), "cran")
```

### Create instance

```
repo <- PackageRepository$new(root)
```

### Create/ensure repository

```
repo$ensure()
```

## Verify existence 

```
repo$exists()
```

### Register and unregister in R options

Register:

```
getOption("repos")
repo$register()
getOption("repos")
```

Unegister:

```
getOption("repos")
repo$register()
getOption("repos")
repo$unregister()
getOption("repos")
```

You can also reset the repos in the R options to its initial state:

```
tmp_1 <- PackageRepository$new("a/b/c")
tmp_2 <- PackageRepository$new("1/2/3")
tmp_1$register()
tmp_2$register()
getOption("repos")
tmp_1$unregister(reset = TRUE)
getOption("repos")
```
### Browse content 

```
repo$browse()
```

### Built into 

```
repo$buildInto()
```

### Investigate

Show content based on index file:

```
repo$show()
repo$show(type = "source")
repo$show(type = "mac.binary")
repo$show(type = "win.binary")
```

Check if specific package(s) exist(s):

```
repo$has()
repo$has(type = "source", atomic = FALSE)
repo$has(c("devtools", "dplyr"))
```

### Maintain

Remove outdated packages and refresh. Outdated packages are moved to an 
special repository archive `repo$root_archive`. 

Each outdated package build will be integrated into its own "one-package-only"  repository: `file.path(repo$root_archive, "<pkg_name>", "<pgk_version>")`

```
repo$clean()
```

Remove packages:

```
repo$remove()
repo$remove("<pkg_name>")
```

Reset entire repository:

```
repo$reset()
```

### Delete

```
repo$exists()
repo$delete()
repo$exists()
```
