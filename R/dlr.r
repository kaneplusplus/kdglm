
#' @export
dlr_predict <- function(data, dlr_model, type) {
  UseMethod("dlr_predict", dlr_model)
}

#' @export
dlr_predict.default <- function(data, dlr_model, type) {
  stop(red("Don't know how to predict with model of type",
           paste0(class(dlr_model), collapse = " ")))
}

#' @importFrom stats model.frame model.matrix predict
#' @export
dlr_predict.continuous_dlr <- function(data, dlr_model, type = NULL) {

  mm <- model.matrix(dlr_model$form, 
                     model.frame(dlr_model$form, 
                                 data[,dlr_model$var_desc$name,
                                      drop = FALSE]))
  predict(dlr_model$model, mm)
}

#' @importFrom crayon red
#' @export
predict.dlr <- function(object, ...) { #newdata, type = "factor") {
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
  dlr_predict(newdata, object, type)
}

#' @importFrom stats model.frame model.matrix predict
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

#' @importFrom tibble tibble
#' @importFrom stats approxfun
survival_features <- function(time, event) {
  ft <- tibble(time = time, event = event) 
  ft <- ft[order(ft$time),]

  # lifetime distribution function
  ret <- tibble(time = double(), lifetime_dist = double())
  time_split <- split(seq_len(nrow(ft)), as.factor(ft$time))
  for (i in seq_along(time_split)) {
    time <- as.double(names(time_split)[i])
    prop <- sum(ft$event[ft$time <= time] == 1) / length(ft$event)
    ret <- rbind(ret, tibble(time = time, lifetime_dist = prop))
  }
  ret$survival_dist <- 1 - ret$lifetime_dist
  ret$hazard <- -log(ret$survival_dist)
  ret
}


#' @importFrom fu dep_terms extract_vars indep_terms
#' @importFrom crayon red
#' @importFrom keras loss_mean_squared_error
#' @export
dlcp <- function(data,
                 form,
                 hidden_layers = integer(),
                 hidden_layers_activation = 
                  rep("linear", length(hidden_layers)),
                 loss = neg_log_prop_haz_lik,
                 optimizer = optimizer_adadelta(),
                 metrics = c("mean_squared_error"),
                 batch_size = nrow(data),
                 epochs = 1000,
                 verbose = FALSE,
                 validation_split = 0.2,
                 name = NULL,
                 ...) {

  # Get the time and event variables to create the hazard function.
  surv_vars <- extract_vars(dep_terms(form))
  indep_vars <- indep_terms(form, data)

  xf <- model.frame(
    as.formula(paste("~", paste(c(surv_vars, indep_vars), collapse = " + "))), 
    data)

  
  # I just want the cleaned up data frame. I don't want the term attributes or else
  # I'll get an error when I try to make a model matrix with another formula.
  attributes(xf)$terms <- NULL

  var_desc <- make_variable_desc(xf, form)
  check_at_least_one_indep_var(var_desc)
  conditional_not_yet_supported(var_desc)
  check_hidden_layers(hidden_layers, hidden_layers_activation)


  # The Nelson-Aalen estimate of the hazard function.
  #haz <- muhaz(xf[[surv_vars[1]]], xf[[surv_vars[[2]]]],
  #             n.est.grid = 2*nrow(xf),
  #             bw.grid = xf[[surv_vars[1]]],
  #             min.time = 0,
  #             max.time = max(xf[[surv_vars[1]]]))
  #sf <- tibble(time = xf[[surv_vars[1]]],
  #             hazard = approxfun(x = haz$est.grid, y = haz$haz.est)(time))

  # TODO: refactor this. There is a lot of overlap with the dlr function.
  x_train <- model.matrix(form, xf)
  mm_column_var_assign <- attributes(x_train)$assign
  column_var_name <- names(xf)
  mm_column_var_name <- colnames(x_train)


  model <- 
    create_input_and_hidden_layers(
      x_train,
      hidden_layers,
      hidden_layers_activation,
      use_bias,
      name)

  input_shape <- NULL
  if (length(hidden_layers) == 0) {
    input_shape <- ncol(x_train)
  }

  model %>%
    layer_dense(units = 1, input_shape = input_shape, name = "hazard_output",
                use_bias = TRUE, activation = "exponential")

  type <- "hazard_dlr"
  y_train <- matrix(c(xf[[surv_vars[1]]], xf[[surv_vars[2]]]), ncol = 2)

  mm_col_names <- colnames(x_train)

  compile(model, loss = loss, optimizer = optimizer, metrics = metrics) 

  history <- 
    fit(model, x_train, y_train, batch_size = batch_size, epochs = epochs,
        validation_split = validation_split, verbose = verbose)

  ret <- list(form = form, 
              hidden_layers = hidden_layers, 
              hidden_layers_activation = hidden_layers_activation,
              column_var_name = column_var_name,
              mm_column_var_name = mm_column_var_name,
              mm_column_var_assign = mm_column_var_assign,
              loss = loss,
              var_desc = var_desc, 
              model = model, 
              history = history,
              mm_col_names = mm_col_names)

  class(ret) <- c(type, "dlr")
  ret
}


#' Create a Supervised Deep Learning Model
#' @importFrom fu make_variable_desc
#' @importFrom keras keras_model_sequential layer_dense %>% compile fit
#' optimizer_adadelta
#' @export
dlr <- function(data, 
                form, 
                hidden_layers = integer(), 
                hidden_layers_activation = 
                  rep("linear", length(hidden_layers)),
                use_bias = rep(TRUE, length(hidden_layers)),
                categorical_loss = loss_categorical_crossentropy,
                continuous_loss = loss_mean_squared_error,
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
  check_hidden_layers(hidden_layers, hidden_layers_activation)

  x_train <- model.matrix(form, xf)
  mm_column_var_assign <- attributes(x_train)$assign
  column_var_name <- names(xf)
  mm_column_var_name <- colnames(x_train)

  model <- 
    create_input_and_hidden_layers(
      x_train, 
      hidden_layers,
      hidden_layers_activation,
      use_bias,
      name)

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
          activation = output_activation,
          use_bias = TRUE) 
    
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
                  name = "linear_output", use_bias = TRUE)

    type <- "continuous_dlr"
    loss <- continuous_loss
    y_train <- as.matrix(xf[[var_desc$name[var_desc$role == "dependent"]]])
  }

  mm_col_names <- colnames(x_train)
  
  model %>% 
    compile(loss = loss, optimizer = optimizer, metrics = metrics) 

  history <- model %>%
    fit(x_train, y_train, batch_size = batch_size, epochs = epochs,
        validation_split = validation_split, verbose = verbose)

  ret <- list(form = form, 
              hidden_layers = hidden_layers, 
              hidden_layers_activation = hidden_layers_activation,
              column_var_name = column_var_name,
              mm_column_var_name = mm_column_var_name,
              mm_column_var_assign = mm_column_var_assign,
              loss = loss,
              var_desc = var_desc, 
              model = model, 
              history = history,
              mm_col_names = mm_col_names)
  class(ret) <- c(type, "dlr")
  ret
}

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
metric_space_embedding.dlr <- function(x, model, 
  layer = length(model$hidden_layers), ...) {

  if (!inherits(x, "data.frame")) {
    stop(red("Don't know how to embed an object of type: ",
             paste(class(x), collapse = " ")))
  }
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

#' @export
plot_training_history <- function(x, metrics, smooth, theme_bw) {
  UseMethod("plot_training_history", x)
}

#' @importFrom crayon red
#' @export
plot_training_history.default <- function(x, metrics, smooth, theme_bw) {
  print(red("Don't know how to plot training history for an object of type:",
            paste(class(x), collapse = " ")))
}

#' @importFrom ggplot2 ggplot aes aes_ geom_point scale_shape theme_bw 
#' theme_minimal geom_smooth facet_grid element_text scale_x_continuous theme
#' element_blank element_rect
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
  do_validation <- any(grepl("^val_", names(x$history$metrics)))
  
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
