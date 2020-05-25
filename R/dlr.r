
#' @importFrom fu make_variable_desc
#' @importFrom keras keras_model_sequential layer_dense %>% compile fit
#' optimizer_adadelta
#' @export
dlr <- function(data, form, hidden_layers = integer(), 
                categorical_loss = "categorical_crossentropy",
                continuous_loss = "mse",
                optimizer = optimizer_adadelta(),
                metrics = "accuracy",
                batch_size = nrow(data),
                epochs = 10,
                verbose = 1,
                validation_split = 0.2) {
  xf <- model.frame(form, data)
  var_desc <- make_variable_desc(xf, form)
  if (sum(var_desc$role == "dependent") > 1) {
    stop(red("You may only have one dependent variable."))
  }
  if (sum(var_desc$role == "independent") == 0) {
    stop(red("You must have at least one independent variable."))
  }
  if (sum(var_desc$role == "conditional") > 0) {
    stop(red("Conditional variables are not currently supported."))
  }

  if (!any(
    var_desc$class[var_desc$role == "dependent"] %in% c("numeric", "factor"))) {
    
    stop(red("Unsupported dependent variable type:", 
             class(var_desc$class[var_desc$role == "dependent"])))
  }

  model <- keras_model_sequential() 
  for (i in seq_along(hidden_layers)) {
    if (i == 1) {
      model %>% layer_dense(input_shape = ncol(model.matrix(form, xf)),
                            units = hidden_layers[i],
                            name = paste("hidden_layer", i, sep = "_"))
    } else {
      model %>% layer_dense(units = hidden_layers[i], 
                            name = paste("hidden_layer", i, sep = "_"))
    }
  }

  if (var_desc$class[var_desc$role == "dependent"] == "factor") {
    output_activation <- "softmax"
    # Are the encodings other than one-hot to consider?  
    oh <- make_one_hot(xf[[var_desc$var_name[var_desc$role == "dependent"]]])
    if (length(hidden_layers) == 0) {
      model %>% 
        layer_dense(
          units=length(var_desc$levels[var_desc$role == "dependent"][[1]]),
          input_shape = ncol(model.matrix(form, xf)))
    } else {
      model %>% 
        layer_dense(
          units=length(var_desc$levels[var_desc$role == "dependent"][[1]]))
    }
    type <- "categorical_dlr"
    loss <- categorical_loss
    y_train <- 
      to_one_hot(xf[[var_desc$var_name[var_desc$role == "dependent"]]],
                   oh)
  } else {
    if (length(hidden_layers) == 0) {
      model %>% 
        layer_dense(units = 1, input_shape = ncol(model.matrix(form, xf)))
    } else {
      model %>% layer_dense(units = 1)
    }
    type <- "continuous_dlr"
    loss <- continuous_loss
    y_train <- as.matrix(xf[[var_desc$var_name[var_desc$role == "dependent"]]])
  }

  x_train <- model.matrix(form, xf)
  model %>% 
    compile(loss = loss, optimizer = optimizer, metrics = "accuracy") %>%
    fit(x_train, y_train, batch_size = batch_size, epochs = epochs,
        validation_split = validation_split, verbose = verbose)

  ret <- list(form = form, hidden_layers = hidden_layers, loss = loss,
              var_desc = var_desc, model = model)
  class(ret) <- c(type, "dlr")
  ret
}

