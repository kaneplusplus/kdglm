
#' @importFrom crayon red
error_on_more_than_one_dep_var <- function(x) {
  if (sum(x$role == "dependent") > 1) {
    stop(red("You may only have one dependent variable."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
error_on_no_indep_var <- function(x) {
  if (sum(x$role == "independent") == 0) {
    stop(red("You must have at least one independent variable."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
error_on_conditional_var <- function(x) {
  if (sum(x$role == "conditional") > 0) {
    stop(red("Conditional variables are not currently supported."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
error_on_unsupported_dependent_var <- function(x, dependent_types) {
  if (!any(x$class[x$role == "dependent"] %in% dependent_types)) {
    stop(red("Unsupported dependent variable type:",
             class(x$class[x$role == "dependent"])))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
#' @export
error_on_bad_hidden_layer_desc <- 
  function(hidden_layers, hidden_layers_activation) {

  if (length(hidden_layers) != length(hidden_layers_activation)) {
    stop(red("hidden_layers and their activations must have the same length."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
error_if_not_model_matrix <- function(mm) {
  if (!all(c("dim", "dimnames", "assign", "contrasts") %in%
           names(attributes(mm)))) {
    stop(red("Argument mm must be a model matrix."))
  }
  invisible(TRUE)
}

#' @importFrom crayon red
error_if_not_data_frame <- function(df) {
  if (!inherits(df, "data.frame")) {
    stop(red("Argument df must be inherited from a data.frame."))
  }
  invisible(TRUE)
}
