
#' @export
dglm_predict <- function(data, dglm_model, type) {
  UseMethod("dglm_predict", dglm_model)
}

#' @export
dglm_predict.default <- function(data, dglm_model, type) {
  stop(red("Don't know how to predict with model of type",
           paste0(class(dglm_model), collapse = " ")))
}

#' @importFrom stats model.frame model.matrix predict
#' @export
dglm_predict.continuous_dglm <- function(data, dglm_model, type = NULL) {
  mm <- model.matrix(dglm_model$formula, 
                     model.frame(dglm_model$formula, 
                                 data[,dglm_model$var_desc$name,
                                      drop = FALSE]))
  predict(dglm_model$model, mm[,-1, drop = FALSE])
}

#' @importFrom crayon red
#' @export
predict.dglm <- function(object, ...) { #newdata, type = "factor") {
  args <- list(...)
  if ("newdata" %in% names(args)) {
    newdata <- args$newdata
  } else if (inherits(args[[1]], "data.frame")) {
    newdata <- args[[1]]
  } else {
    stop(red("You must specify a newdata object to predict on."))
  }
  type <- "factor"
  if ( "type" %in% names(args) ) {
    type <- args$type
  }
  dglm_predict(newdata, object, type)
}

#' @importFrom stats model.frame model.matrix predict
#' @export
dglm_predict.categorical_dglm <- function(data, dglm_model, type = "factor") {
  mm <- model.matrix(dglm_model$formula, 
                     model.frame(dglm_model$formula, 
                                 data[,dglm_model$var_desc$name,
                                      drop = FALSE]))
  res <- predict(dglm_model$model, mm[,-1, drop = FALSE])
  var_desc <- dglm_model$var_desc
  colnames(res) <- var_desc$levels[var_desc$role == "dependent"][[1]]
  if (type == "factor") {
    res <- colnames(res)[apply(res, 1, which.max)]
  }
  res
}

#' @export
dglm_predict.survival_dglm <- function(data, dglm_model, type = NULL) {
  mm <- model.matrix(dglm_model$formula, 
                     model.frame(dglm_model$formula, data))
  predict(dglm_model$model, mm[,-1, drop = FALSE])
}
