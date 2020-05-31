
#' @export
dlr_predict <- function(data, dlr_model, type) {
  UseMethod("dlr_predict", dlr_model)
}

#' @export
dlr_predict.default <- function(data, dlr_model, type) {
  stop(red("Don't know how to predict with model of type",
           paste0(class(dlr_model), collapse = " ")))
}

#' @export
dlr_predict.continuous_dlr <- function(data, dlr_model, type = NULL) {

  mm <- model.matrix(dlr_model$form, 
                     model.frame(dlr_model$form, 
                                 data[,dlr_model$var_desc$name,
                                      drop = FALSE]))
  predict(dlr_model$model, mm)
}

#' @export
predict.dlr <- function(object, newdata, type = "factor") {
  dlr_predict(newdata, object, type)
}

#' pre

#' @export
dlr_predict.categorical_dlr <- function(data, dlr_model, type = "factor") {
  mm <- model.matrix(dlr_model$form, 
                     model.frame(dlr_model$form, 
                                 data[,dlr_model$var_desc$name,
                                      drop = FALSE]))
  res <- predict(dlr_model$model, mm)
  var_desc <- dlr_model$var_desc
  colnames(res) <- var_desc$levels[var_desc$role == "dependent"][[1]]
  if (type == "factor") {
    res <- colnames(res)[apply(res, 1, which.max)]
  }
  res
}

#' Create a Supervised Deep Learning Model
#' @importFrom fu make_variable_desc
#' @importFrom keras keras_model_sequential layer_dense %>% compile fit
#' optimizer_adadelta
#' @export
dlr <- function(data, 
                form, 
                hidden_layers = integer(), 
                categorical_loss = "mse",
                continuous_loss = "mse",
                optimizer = optimizer_adadelta(),
                metrics = c("accuracy", "mean_squared_error"),
                batch_size = nrow(data),
                epochs = 1000,
                verbose = FALSE,
                validation_split = 0.2,
                name = NULL) {

  xf <- model.frame(form, data)

  var_desc <- make_variable_desc(xf, form)

  check_only_one_dependent_var(var_desc)
  check_at_least_one_indep_var(var_desc)
  conditional_not_yet_supported(var_desc)
  check_dependent_types(var_desc, c("numeric", "factor"))

  x_train <- model.matrix(form, xf)

  model <- keras_model_sequential(name = name) 

  for (i in seq_along(hidden_layers)) {
    if (i == 1) {
      model %>% layer_dense(input_shape = ncol(x_train),
                            units = hidden_layers[i],
                            name = paste("hidden_layer", i, sep = "_"),
                            use_bias = FALSE)
    } else {
      model %>% layer_dense(units = hidden_layers[i], 
                            name = paste("hidden_layer", i, sep = "_"),
                            use_bias = FALSE)
    }
  }

  if (var_desc$class[var_desc$role == "dependent"] == "factor") {

    output_activation <- "softmax"
    # Are the encodings other than one-hot to consider?  
    oh <- make_one_hot(xf[[var_desc$name[var_desc$role == "dependent"]]])

    input_shape <- NULL
    if (length(hidden_layers) == 0) {
      input_shape <- ncol(x_train)
    }

    model %>% 
      layer_dense(
          units=length(var_desc$levels[var_desc$role == "dependent"][[1]]),
          input_shape = input_shape,
          name = paste(output_activation, "output", sep = "_"),
          use_bias = FALSE)
    
    type <- "categorical_dlr"
    loss <- categorical_loss
    y_train <- 
      to_one_hot(xf[[var_desc$name[var_desc$role == "dependent"]]],
                   oh)
  } else {
    input_shape <- NULL
    if (length(hidden_layers) == 0) {
      input_shape <- ncol(x_train)
    }

    model %>% 
      layer_dense(units = 1, input_shape = input_shape, 
                  name = "linear_output", use_bias = FALSE)

    type <- "continuous_dlr"
    loss <- continuous_loss
    y_train <- as.matrix(xf[[var_desc$name[var_desc$role == "dependent"]]])
  }

  mm_col_names <- colnames(x_train)
  model %>% compile(loss = loss, optimizer = optimizer, metrics = "accuracy") 
  history <- model%>%
    fit(x_train, y_train, batch_size = batch_size, epochs = epochs,
        validation_split = validation_split, verbose = verbose)

  ret <- list(form = form, hidden_layers = hidden_layers, loss = loss,
              var_desc = var_desc, 
              model = model, 
              history = history,
              mm_col_names = mm_col_names)
  class(ret) <- c(type, "dlr")
  ret
}

#' @export
metric_space_embedding <- function(x, model, layer) {
  UseMethod("metric_space_embedding", model)
}

#' @importFrom crayon red
#' @export
metric_space_embedding.default <- function(x, model, layer) {
  stop(red("Don't know how to calculate metric space embedding for an object ",
           "of type: ", paste(class(x), collapse = " ")))
}

#' @importFrom keras get_weights
#' @export
metric_space_embedding.dlr <- function(x, model, 
  layer = length(model$hidden_layers)) {

  mm <- model.matrix(model$form, 
                     model.frame(model$form, 
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
  last_weights <- get_weights(model$model)[[layer + 1]]
  ret <- Reduce(cbind, 
         Map(function(j) sweep(pmm, 2, last_weights[,j], FUN = `*`), 
             seq_len(ncol(last_weights))))
  dimnames(ret) <- NULL
  attributes(ret)$assign <- NULL
  attributes(ret)$contrasts <- NULL
  ret
}

#' @export
plot_training_history <- function(x) {
  UseMethod("plot_training_history", x)
}

#' @importFrom crayon red
#' @export
plot_training_history.default <- function(x) {
  print(red("Don't know how to plot training history for an object of type:",
            paste(class(x), collapse = " ")))
}

#' @importFrom ggplot2 ggplot aes aes_ geom_point scale_shape theme_bw 
#' theme_minimal
#' @importFrom tools toTitleCase
#' @export
plot_training_history.dlr <- function(x, metrics = NULL, 
    smooth = getOption("keras.plot.history.smooth", TRUE), 
    theme_bw = FALSE) {

  df <- as.data.frame(x$history)
  if (is.null(metrics))
      metrics <- Filter(function(name) !grepl("^val_", name),
          names(x$history$metrics))
  df <- df[df$metric %in% metrics, ]
  if (tensorflow::tf_version() < "2.2") {
      do_validation <- x$history$params$do_validation
  } else {
    do_validation <- any(grepl("^val_", names(x$history$metrics)))
  }
  
  names(df) <- toTitleCase(names(df))
  df$Metric <- toTitleCase(as.character(df$Metric))
  df$Data <- toTitleCase(as.character(df$Data))

  int_breaks <- function(x) pretty(x)[pretty(x)%%1 == 0]
  if (do_validation) {
    if (theme_bw) {
      p <- ggplot(df, 
                  aes_(~Epoch, ~Value, color = ~Data, fill = ~Data, 
                       linetype = ~Data, shape = ~Data))
    } else {
      p <- ggplot(df, aes_(~Epoch, ~Value, color = ~Data, fill = ~Data))
    }
  } else {
    p <- ggplot(df, ggplot2::aes_(~Epoch, ~Value))
  }
  smooth_args <- list(se = FALSE, method = "loess", na.rm = TRUE)
  if (theme_bw) {
    smooth_args$size <- 0.5
    smooth_args$color <- "gray47"
    p <- p + 
      theme_bw() + 
      geom_point(col = 1, na.rm = TRUE, size = 2) + 
      scale_shape(solid = FALSE)
  }
  else {
    p <- p + geom_point(shape = 21, col = 1, na.rm = TRUE)
  }
  if (smooth && x$history$params$epochs >= 10) {
    p <- p + do.call(geom_smooth, smooth_args)
  }
  p + facet_grid(Metric ~ ., switch = "y", scales = "free_y") + 
    scale_x_continuous(breaks = int_breaks) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          strip.placement = "outside", 
          strip.text = element_text(colour = "black", size = 11), 
          strip.background = element_rect(fill = NA, color = NA))
}
