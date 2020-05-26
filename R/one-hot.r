
#' @export
make_one_hot <- function(x) {
  UseMethod("make_one_hot", x)
}

#' @export
make_one_hot.default <- function(x) {
  stop(red("Don't know how to create one_hot from object of type:",
           paste(class(x), collapse = " ")))
}

#' @export
make_one_hot.factor <- function(x) {
  ret <- x[c()]
  class(ret) <- c("one_hot", class(ret))
  ret
}

#' @export
make_one_hot.character <- function(x) {
  warning(yellow("Calling as.factor on character vector."))
  make_one_hot(as.factor(x))
}

#' @export
to_one_hot <- function(x, oh) {
  UseMethod("to_one_hot", oh)
}

#' @export
to_one_hot.default <- function(x, oh) {
  stop(red("Argument oh is not of type one_hot."))
}

#' @importFrom keras to_categorical
#' @importFrom crayon red
#' @export
to_one_hot.one_hot <- function(x, oh) {
  if (!all(levels(x) == levels(oh))) {
    stop(red("Levels are as specified in one_hot."))
  }
  if (is.ordered(x) != is.ordered(oh)) {
    stop(red("Levels do not agree on ordering."))
  }
  ret <- to_categorical(as.integer(x) - 1, num_classes = length(levels(x)))
  colnames(ret) <- levels(x)
  ret
}

#' @export
from_one_hot <- function(x, oh) {
  UseMethod("from_one_hot", oh)
}

#' @importFrom crayon red
#' @export
from_one_hot.default <- function(x, oh) {
  stop(red("Argument oh is not of type one_hot."))
}

#' @importFrom crayon red yellow
#' @export
from_one_hot.one_hot <- function(x, oh) {
  if (!inherits(x, "matrix")) {
    stop(red("Argument x should be a matrix."))
  }
  if (is.null(colnames(x))) {
    warning(yellow("Column names not provided. They will be assumed from oh."))
    if (ncol(x) != length(levels(oh))) {
      stop(red("Incorrect number of columns in argument x."))
    }
  } else {
    if (ncol(x) != length(levels(oh))) {
      stop(red("Incorrect number of columns in argument x."))
    }
    if (!all(levels(x) == levels(oh))) {
      warning(yellow("Matrix columns will be reordered to those of the one hot",
                     "encoding order."))
      x <- x[, levels(x)]
    }
  }
  factor(levels(oh)[apply(x, 1, which.max)], levels = levels(oh))
}

