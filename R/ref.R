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
  e <- new.env(FALSE, parent.frame(), 0L)
  attributes(e) <- list(class = "ref", object = object, constraint = constraint)
  lockEnvironment(e, bindings = TRUE)
  e
}

#' Extract the value of a ref object
#' @param x a ref object
#' @export
#' @examples
#' ref1 <- ref(1:10)
#' deref(ref1)
deref <- function(x) {
  if (inherits(x, "ref")) {
    attr(x, "object", TRUE)
  } else x
}

#' Get the constraint
#' @param x an object
#' @export
constraint <- function(x)
  UseMethod("constraint")

#' @export
constraint.ref <- function(x) {
  attr(x, "constraint", TRUE)
}

#' @export
with.ref <- function(data, expr, ...) {
  object <- deref(data)
  eval(substitute(expr), if (is.list(object)) object else NULL, data)
}

fupdate <- function(f) {
  function(x, value) {
    obj <- deref(x)
    update.ref(x, f(obj, value))
  }
}

#' @export
length.ref <- function(x) length(deref(x))

#' @export
`length<-.ref` <- fupdate(`length<-`)

#' @export
names.ref <- function(x) names(deref(x))

#' @export
`names<-.ref` <- fupdate(`names<-`)

#' @export
row.names.ref <- function(x) rownames(deref(x))

#' @export
`row.names<-.ref` <- fupdate(`rownames<-`)

#' @export
#' @importFrom utils head
head.ref <- function(x, ...) NextMethod("head", deref(x), ...)

#' @export
#' @importFrom utils tail
tail.ref <- function(x, ...) NextMethod("tail", deref(x), ...)

#' @export
as.numeric.ref <- function(x, ...) as.numeric(deref(x), ...)

#' @export
as.integer.ref <- function(x, ...) as.integer(deref(x), ...)

#' @export
as.character.ref <- function(x, ...) as.character(deref(x), ...)

#' @export
as.list.ref <- function(x, ...) as.list(deref(x), ...)

#' @export
as.complex.ref <- function(x, ...) as.complex(deref(x), ...)

#' @export
as.raw.ref <- function(x) as.raw(deref(x))

#' @export
as.data.frame.ref <- function(x, ...) as.data.frame(deref(x), ...)

#' @export
as.matrix.ref <- function(x, ...) as.matrix(deref(x), ...)

#' @export
as.array.ref <- function(x, ...) as.array(deref(x), ...)


validate <- function(x, object) {
  constraint <- constraint.ref(x)
  is.null(constraint) || constraint(object)
}

#' @export
print.ref <- function(x, ...) {
  object <- deref(x)
  cat(sprintf("<ref: %s>\n", paste0(class(object), collapse = ", ")))
  cat(format.default(x), "\n")
  if (!is.null(object)) print(object, ...)
  invisible(x)
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
  if (!validate(object, value)) 
    stop("ref constraint is violated by supplied value", call. = FALSE)
  attr(object, "object") <- value
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

