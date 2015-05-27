# CHANGES IN reposr VERSION 0.2.9

## NEW FEATURES

## BUG FIXES

- fixed: #14 (missing binary subdir existence check)

## MAJOR CHANGES

- M: `delete()` now has argument `subdir` to restrict scope to specific subdirectories
- M: `exists()` now has argument `subdir` to restrict scope to specific subdirectories
- M: `ensure()` now has argument `subdir` to restrict scope to specific subdirectories
- M: `ensureIndexFiles()` now has argument `subdir` to restrict scope to specific subdirectories

## MINOR CHANGES

- M: renamed `respondsUrl()` to `isRespondingUrl()`

## MISC

## TODO

-----

# CHANGES IN reposr VERSION 0.2.8

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.2

## NEW FEATURES

- `$asUrl()` (https://github.com/rappster/reposr/issues/3)
- `$removePackage()` (https://github.com/rappster/reposr/issues/5)

## BUG FIXES

## MAJOR CHANGES

- The package has been radically refactored. The entire functionality is now based on class `PackageRepository`.

## MINOR CHANGES

- Return value consistency (https://github.com/rappster/reposr/issues/6)

## MISC

-----

# CHANGES IN VERSION 0.1.4

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- cleanup in order to use git-flow paradigm

-----

# CHANGES IN VERSION 0.1.3

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reposr VERSION 0.1.0.10

## NEW FEATURES

## BUG FIXES

- normalization of repository paths where necessary:
  - `asRepository()`
  - `asExpandedRepository()`
  - `ensureRepository()`
  - `getExpandedRepositoryPaths()`

## MAJOR CHANGES

## MINOR CHANGES

## MISC 

- new patch version to comply with repo conventions

-----

# CHANGES IN reposr VERSION 0.1.0.9

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC 

- new patch version to comply with repo conventions

-----

# CHANGES IN reposr VERSION 0.1.0.8

## NEW FEATURES

## BUG FIXES

- ensured that `signalCondition()` from namespace `conditionr` is used

## MAJOR CHANGES

## MINOR CHANGES
