reposr (0.2.7)
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

### Remote repositories

Note that, even though the package has **not** been specifically designed to express non-local repositories, remote repositories can also be handled. However, keep in mind that not all methods will work for remote repositories (e.g. `$show()`)

```
repo_http <- PackageRepository$new("http://cran.rstudio.com")
repo_http
repo_http$scheme

repo_ftp <- PackageRepository$new("ftp://cran.at.r-project.org/")
repo_ftp
repo_ftp$scheme
```

### Create/ensure repository

```
repo$ensure()
```

Note that you could also ensure the repository without its index files by setting `index = FALSE`.

#### Ensure atomic repositories

The package offers a built-in way to create atomic, i.e. package-version based package repositories. This is important/can be helpful in two scenarios:

1. When developing, you typically follow the following cycle:

    1. Build current package version directly into the repository with `$buildInto()`
    2. Make changes which is/should be reflected in a new version number 
    3. Rebuild updated version 
    
    While your repository will at some point thus contain multiple actual builds, be aware that only the latest build will be reflected in the repository index files. Thus, when you decide to "clean up" at some point, running `$clean(archive = TRUE)`) will make sure that all the outdated builds are not lost but moved to the respective atomic package repository.
    
2. Method `$atomize()` atomarizes the content of a repository in the sense that a own repository for each package's version is ensured below a root directory.

The actual directory structure of the collection of atomic repositories is as follows:

```
root
  /pkg_a
    /1.0
    /1.1
    /...
    /x.y.z
    /...
    /x.y
  /pkg_b
    /1.0
    /1.1
    /...
    /x.y.z
    /...
    /x.y
  /...
```

Ideally, the actual files contained in atomic repostories would simply be soft symbolic links in order to avoid saving packages redundantly. However, so far I could not manage to realize this (see [SO post](http://stackoverflow.com/questions/28838859/creating-a-soft-symbolic-link-from-r-on-windows))

repo$ensure(atomic = TRUE)

## Verify existence 

```
repo$exists()
repo$exists(atomic = TRUE)
```

### Register and unregister in R options

Register:

```
getOption("repos")
repo$register()
getOption("repos")
```

See what's currently registered:

```
repo$showRegistered()
repo$showRegistered(custom_only = TRUE)
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

You can also directly move into the subdirectories:

```
repo$browse("source")
repo$browse("mac.binary")
repo$browse("win.binary")
```

### Built into 

Build package directly into the repository

```
repo$buildInto()
```

### Investigate

#### Show 
Show content based on index file:

```
repo$show()
repo$show(type = "source")
repo$show(type = "mac.binary")
repo$show(type = "win.binary")
```

#### Has

Check if specific package(s) exist(s):

```
repo$has()
repo$has(type = "source", atomic = FALSE)
repo$has(c("devtools", "dplyr"))
```

### Maintain

Remove outdated packages and refresh. If `archive = TRUE`, outdated packages are moved to a special repository archive `repo$root_atomic`. 

Each outdated package build will be integrated into its own "package-version-specific"  repository: `file.path(repo$root_atomic, "<pkg_name>", "<pgk_version>")`

```
repo$clean(archive = TRUE)
```

#### Remove 

Remove current package from all subdirectories:

```
repo$remove()
```

Remove specific package:

```
repo$remove("<pkg_name>")
```

Reset entire repository:

```
repo$reset()
```

### Pull dependencies

Pull all packages that your own package depens on from registered repositories:

```
repo$pull()
```

It is also possible to pull an arbitrary package from a repository (which can either be one of the registerd ones or a vector of explicit repositories specified via the `repos` argument):

```
repo$pull(c("pkgKitten", "stringr")
```

Ensure that repository is atomized after pull (atomic package-version-specific repositories below repository archive (`repo$browse(atomic = TRUE)`)):

```
repo$pull(atomize = TRUE)
```

### Atomize (explicit)

Atomize repository content so each package version gets its version-specific repository in the repository archive:

```
repo$atomize()
```

Browse repository archive:

```
repo$browse(atomic = TRUE)
```

### Push 

Push entire repository:

```
to <- file.path(tempdir(), "cran_2")
repo$push(to = to)
repo_2 <- PackageRepository$new(to)
repo_2$browse()
```

Push package(s):

```
to <- file.path(tempdir(), "cran_3")
repo$push(pkg = "reposr", to = to)
repo_2 <- PackageRepository$new(to)
repo_2$browse()
repo_2$delete(ask = FALSE)
```

### Delete

```
repo$exists()
repo$delete()
repo$exists()
```
