

#' Create a Supervised Deep Learning Model
#' @importFrom fu make_variable_desc
#' @importFrom keras keras_model_sequential layer_dense %>% compile fit
#' optimizer_adadelta loss_mean_squared_error loss_categorical_crossentropy
#' @export
dglm <- function(data, 
                 formula, 
                 hidden_layers = integer(), 
                 hidden_layers_activation = 
                   rep("linear", length(hidden_layers)),
                 hidden_layer_names = 
                   paste("hidden_layer", seq_along(hidden_layers), sep = "_"),
                 use_bias = rep(TRUE, length(hidden_layers)),
                 loss = NULL,
                 optimizer = optimizer_adadelta(),
                 metrics = NULL, 
                 output_activation = NULL,
                 output_activation_bias = TRUE,
                 batch_size = nrow(data),
                 epochs = 1000,
                 verbose = FALSE,
                 validation_split = 0.2,
                 count_model = FALSE,
                 name = NULL) {

  xf <- model.frame(formula, data)

  var_desc <- make_variable_desc(xf, formula)

  error_on_more_than_one_dep_var(var_desc)
  error_on_no_indep_var(var_desc)
  error_on_conditional_var(var_desc)
  error_on_unsupported_dependent_var(var_desc, c("numeric", "factor"))
  error_on_bad_hidden_layer_desc(hidden_layers, hidden_layers_activation)

  x_train <- make_model_matrix(formula, xf)

  # make sure there is an intercept, if not warning
  mm_column_var_assign <- attributes(x_train)$assign
  column_var_name <- names(xf)
  mm_column_var_name <- colnames(x_train)

  model <- 
    create_input_and_hidden_layers(
      x_train, 
      hidden_layers,
      hidden_layers_activation,
      hidden_layer_names,
      use_bias,
      name)

  if (var_desc$class[var_desc$role == "dependent"] == "factor") {

   #              categorical_loss = loss_categorical_crossentropy,
   #              continuous_loss = loss_mean_squared_error,
   # output_activation <- "softmax"

    if (is.null(loss)) {
      loss <- loss_categorical_crossentropy
    }
    if (is.null(metrics)) {
      metrics
    }

    if (is.null(output_activation)) {
      output_activation <- "softmax"
    }

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
          use_bias = output_activation_bias) 
    
    type <- "categorical_dglm"
    loss <- loss_categorical_crossentropy
    y_train <- 
      to_one_hot(xf[[var_desc$name[var_desc$role == "dependent"]]],
                   oh)
  } else if (var_desc$class[var_desc$role == "dependent"] == "numeric") {
    input_shape <- NULL
    if (length(hidden_layers) == 0) {
      input_shape <- ncol(x_train)
    }

    if (is.null(loss)) {
      loss <- loss_mean_squared_error
    }
    model %>% 
      layer_dense(units = 1, input_shape = input_shape, 
                  name = "continuous_output", use_bias = TRUE,
                  activation = output_activation)

    type <- "continuous_dglm"
    y_train <- as.matrix(xf[[var_desc$name[var_desc$role == "dependent"]]])
  } else {
    stop("Unsupported dependent variable type.")
  }

  if (is.null(metrics)) {
    metrics <- loss
  }

  mm_col_names <- colnames(x_train)
  
  model %>% 
    compile(loss = loss, optimizer = optimizer, metrics = metrics) 

  history <- model %>%
    fit(x_train, y_train, batch_size = batch_size, epochs = epochs,
        validation_split = validation_split, verbose = verbose)

  ret <- list(formula = formula, 
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
  class(ret) <- c(type, "dglm")
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
plot_training_history.dglm <- function(x, metrics = NULL, 
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
