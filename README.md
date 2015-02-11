repor (0.2)
======

Local Package Repository Management

## Installation

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/repor")
require("repor")
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

### Register as R option

```
(getOption("repos"))
repo$register()
(getOption("repos"))
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
