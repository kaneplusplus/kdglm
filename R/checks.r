
check_only_one_dependent_var <- function(x) {
  if (sum(x$role == "dependent") > 1) {
    stop(red("You may only have one dependent variable."))
  }
}

check_at_least_one_indep_var <- function(x) {
  if (sum(x$role == "independent") == 0) {
    stop(red("You must have at least one independent variable."))
  }
}

conditional_not_yet_supported <- function(x) {
  if (sum(x$role == "conditional") > 0) {
    stop(red("Conditional variables are not currently supported."))
  }
}

check_dependent_types <- function(x, dependent_types) {
  if (!any(x$class[x$role == "dependent"] %in% dependent_types)) {
    stop(red("Unsupported dependent variable type:",
             class(x$class[var_desc$role == "dependent"])))
  }
}
