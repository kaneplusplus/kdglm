
#' @export
metric_space_embedding <- function(x, model, layer, ...) {
  UseMethod("metric_space_embedding", model)
}

#' @importFrom keras keras_model get_layer get_weights
#' @importFrom crayon red
#' @export
metric_space_embedding.default <- function(x, model, layer, ...) {
  stop(red("Don't know how to calculate metric space embedding with model ",
           "of type: ", paste(class(model), collapse = " ")))
}

#' @importFrom keras get_weights
#' @export
metric_space_embedding.dglm <- function(x, model, 
  layer = length(model$hidden_layers), ...) {

  if (!inherits(x, "data.frame")) {
    stop(red("Don't know how to embed an object of type: ",
             paste(class(x), collapse = " ")))
  }
  mm <- model.matrix(model$formula, 
                     model.frame(model$formula, 
                                 x[,model$var_desc$name,
                                      drop = FALSE]))
  
  if (layer > 0 && length(model$hidden_layers) > 0) {
    lsm <- keras_model(inputs = model$model$inputs, 
      outputs = get_layer(model$model, 
                          paste("hidden_layer", layer, sep ="_"))$output)
    pmm <- predict(lsm, mm)
  } else if (layer == 0) {
    pmm <- mm
  } else {
    stop(red("Invalid layer."))
  }
  last_weights <- get_weights(model$model)[[2*(layer + 1) - 1]]
  last_bias <- get_weights(model$model)[[2*(layer + 1)]]
  ret <- Reduce(cbind, 
         Map(
           function(j) {
             cbind(sweep(pmm, 2, last_weights[,j], FUN = `*`), last_bias[j])
           }, seq_len(ncol(last_weights))))

  args <- list(...)
  # See if we should remove the bias term.
  if (!("keep_bias" %in% names(args) && isTRUE(args$keep_bias))) {
    ret <- ret[,-ncol(ret)]
  } 
  dimnames(ret) <- NULL
  attributes(ret)$assign <- NULL
  attributes(ret)$contrasts <- NULL
  ret
}

