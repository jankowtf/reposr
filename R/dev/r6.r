## Issue #39
## https://github.com/wch/R6/issues/39

MyClass <- R6Class(
  classname = "MyClass",
  public = list(
    x = "character",
    initialize = function(
      x = "a"
    ) {
      self$x <- x
    }
  ),
  active = list(
    y = function() file.path(self$x, "b")
  )
)
inst <- MyClass$new()
inst$x
inst$y
inst$y <- "a/b/c"

makeActiveBinding(
  "test", 
  env = .GlobalEnv,
  function(v) {
    if (missing(v)) {
      print("Execute the binding function")
    } else {
      stop("You tried to assign a value to an active field that does not support this (strict dependency)")
    }
  }
)
test
test <- "a"

################################################################################

## Issue #40
## https://github.com/wch/R6/issues/40

MyClass <- R6Class(
  classname = "MyClass",
  public = list(
    x = "character",
    initialize = function(
      x = "a"
    ) {
      self$x <- x
    }
  ),
  private = list(
    y = file.path(self$x, "b"),
    foo = function() {
      "I'm doing something useful"
    }
  )
)
inst <- MyClass$new()
inst$x

self <- inst
private <- inst$.private

MyClass <- R6Class(
  classname = "MyClass",
  public = list(
    x = "character",
    initialize = function(
      x = "a"
    ) {
      self$x <- x
      private$internal_env <- sys.frames()[[1]]
    },
    getInternalEnvironment = function() private$internal_env
  ),
  private = list(
    y = file.path(self$x, "b"),
    foo = function() {
      "I'm doing something useful"
    },
    internal_env = "environment" 
  )
)
inst <- MyClass$new()
env <- inst$getInternalEnvironment()
ls(env)
ls(env$private_bind_env)
private <- env$private_bind_env
private$y
private$foo()

################################################################################

MyClass <- R6Class(
  classname = "MyClass",
  public = list(
    initialize = function(
    ) {
      frames <- sys.frames()
      private$.private_env <- frames[[length(frames) - 1]]$private_bind_env
    },
    getPrivate = function() private$.private_env
  ),
  private = list(
    .private_env = "environment",
    foo = function() {
      "I'm a private function"
    }
  )
)

test_that("private/foo", {
  inst <- MyClass$new()
  expect_identical(inst$getPrivate()$foo(),
    "I'm a private function"
  )
})
