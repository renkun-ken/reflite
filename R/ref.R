#' Create a reference to R object
#'
#' Create a ref as a wrapper of a given object.
#' A ref is essentially a wrapper environment of
#' the object.
#' @param object The object.
#' @param constraint a function to validate the
#' value in each update of the object.
#' @export
#' @examples
#' # create a NULL ref
#' ref()
#'
#' # create a ref of an integer vector
#' x <- ref(1:10)
#' x[1]
#' x[5] <- 10L
#'
#' # create a ref with constraint of an integer vector
#' x <- ref(1:10, is.integer)
#' x[1]
#' x[5] <- 10L
#' # x[5] <- "a" will corece 1:10 to character vector
#' # but violates is.integer contraint, leading to a stop.
ref <- function(object = NULL, constraint = NULL) {
  if (!is.null(constraint) && !constraint(object)) {
    stop("constraint is violated by initial value")
  }
  e <- new.env(FALSE, parent.frame(), 4L)
  list2env(list(object = object, constraint = constraint,
    constraint_expr = substitute(constraint),
    address = sub("<environment: (.*)>", "\\1", format.default(e))), e)
  lockEnvironment(e)
  structure(e, class = "ref")
}

#' Extract the value of a ref object
#' @param x a ref object
#' @export
#' @examples
#' ref1 <- ref(1:10)
#' deref(ref1)
deref <- function(x) {
  if (inherits(x, "ref")) {
    get("object", envir = x, inherits = FALSE)
  } else x
}

#' Get the constraint
#' @param x an object
#' @export
constraint <- function(x)
  UseMethod("constraint")

#' @export
constraint.ref <- function(x) {
  get("constraint", envir = x, inherits = FALSE)
}

validate <- function(x, object) {
  constraint <- constraint.ref(x)
  is.null(constraint) || constraint(object)
}

#' @export
print.ref <- function(x, ...) {
  object <- deref(x)
  address <- get("address", envir = x, inherits = FALSE)
  cat(sprintf("<ref: %s @ %s>\n", paste0(class(object), collapse = ", "), address))
  constraint_expr <- get("constraint_expr", envir = x, inherits = FALSE)
  if (!is.null(constraint_expr)) {
    cat(sprintf("<constraint: %s>\n",
      if (is.symbol(constraint_expr)) as.character(constraint_expr)
      else "function"))
  }
  if (!is.null(object)) {
    print(object, ...)
  }
}

#' Update the value in ref object
#'
#' Update the value in ref object. The constraint (if any) will
#' be validated before any update of the ref object.
#' @param object a ref object
#' @param value the new value
#' @param ... additional parameters currently ignored.
#' @export
#' @examples
#' ref1 <- ref(1:10)
#' update(ref1, rnorm(10))
#'
#' ref2 <- ref(1:10, function(x) length(x) == 10L)
#' update(ref2, rnorm(10))
#' # The following will fail due to the violation
#' # of the constraint.
#' # update(ref2, rnorm(20))
update.ref <- function(object, value, ...) {
  if (!validate(object, value)) stop("ref constraint is violated by supplied value")
  assign("object", value, envir = object, inherits = FALSE)
  invisible(object)
}

#' @export
`+.ref` <- function(x, y) {
  deref(x) + deref(y)
}

#' @export
`-.ref` <- function(x, y) {
  if (missing(y)) -deref(x) else deref(x) - deref(y)
}

#' @export
`*.ref` <- function(x, y) {
  deref(x) * deref(y)
}

#' @export
`/.ref` <- function(x, y) {
  deref(x) / deref(y)
}

#' @export
`^.ref` <- function(x, y) {
  deref(x) ^ deref(y)
}

#' @export
`%%.ref` <- function(x, y) {
  deref(x) %% deref(y)
}

create_get_method <- function(f) {
  function(x, ...) {
    dots <- match.call(expand.dots = FALSE)$...
    callobj <- as.call(c(list(f, quote(x)), dots))
    eval(callobj, list(x = deref(x)), parent.frame())
  }
}

create_set_method <- function(f) {
  function(x, ..., value) {
    dots <- match.call(expand.dots = FALSE)$...
    callobj <- as.call(c(list(f, quote(x)), dots, list(value)))
    obj <- eval(callobj, list(x = deref(x)), parent.frame())
    update.ref(x, obj)
    invisible(x)
  }
}

#' @export
`[.ref` <- create_get_method(quote(`[`))

#' @export
`[[.ref` <- create_get_method(quote(`[[`))

#' @export
`$.ref` <- create_get_method(quote(`$`))

#' @export
`[<-.ref` <- create_set_method(quote(`[<-`))

#' @export
`[[<-.ref` <- create_set_method(quote(`[[<-`))

#' @export
`$<-.ref` <- create_set_method(quote(`$<-`))
