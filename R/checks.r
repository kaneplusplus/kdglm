
#' @importFrom crayon red
check_only_one_dependent_var <- function(x) {
  if (sum(x$role == "dependent") > 1) {
    stop(red("You may only have one dependent variable."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
check_at_least_one_indep_var <- function(x) {
  if (sum(x$role == "independent") == 0) {
    stop(red("You must have at least one independent variable."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
conditional_not_yet_supported <- function(x) {
  if (sum(x$role == "conditional") > 0) {
    stop(red("Conditional variables are not currently supported."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
check_dependent_types <- function(x, dependent_types) {
  if (!any(x$class[x$role == "dependent"] %in% dependent_types)) {
    stop(red("Unsupported dependent variable type:",
             class(x$class[x$role == "dependent"])))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
check_hidden_layers <- function(hidden_layers, hidden_layers_activation) {
  if (length(hidden_layers) != length(hidden_layers_activation)) {
    stop(red("hidden_layers and their activations must have the same length."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
check_is_model_matrix <- function(mm) {
  if (!all(c("dim", "dimnames", "assign", "contrasts") %in%
           names(attributes(mm)))) {
    stop(red("Argument mm must be a model matrix."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
check_inherits_data_frame <- function(df) {
  if (!inherits(df, "data.frame")) {
    stop(red("Argument df must be inherited from a data.frame."))
  }
  invisible(TRUE)
}
