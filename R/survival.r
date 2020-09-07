# @importFrom tibble tibble
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
dcoxph <- function(data,
                   formula,
                   hidden_layers = integer(),
                   hidden_layers_activation = 
                    rep("linear", length(hidden_layers)),
                   use_bias = rep(FALSE, length(hidden_layers) + 1),
                   loss = neg_log_prop_haz_lik,
                   optimizer = optimizer_adadelta(),
                   metrics = neg_log_prop_haz_lik,
                   output_activation = "linear",
                   batch_size = nrow(data),
                   epochs = 1000,
                   verbose = FALSE,
                   validation_split = 0.2,
                   name = NULL,
                   zero_weights = TRUE,
                   ...) {

  # Get the time and event variables to create the hazard function.
  surv_vars <- extract_vars(dep_terms(formula))
  indep_vars <- indep_terms(formula, data)

  xf <- model.frame(
    as.formula(paste("~", paste(c(surv_vars, indep_vars), collapse = " + "))), 
    data)

  
  # I just want the cleaned up data frame. I don't want the term attributes or else
  # I'll get an error when I try to make a model matrix with another formula.
  attributes(xf)$terms <- NULL

  var_desc <- make_variable_desc(xf, formula)
  error_on_no_indep_var(var_desc)
  error_on_conditional_var(var_desc)
  error_on_bad_hidden_layer_desc(hidden_layers, hidden_layers_activation)

  if (any(!(xf[[surv_vars[2]]] %in% c(0, 1)))) {
    stop(red("Event variables should be 0 if right censored or 1 if an event",
             "took place."))
  }

  # The Nelson-Aalen estimate of the hazard function.
  #haz <- muhaz(xf[[surv_vars[1]]], xf[[surv_vars[[2]]]],
  #             n.est.grid = 2*nrow(xf),
  #             bw.grid = xf[[surv_vars[1]]],
  #             min.time = 0,
  #             max.time = max(xf[[surv_vars[1]]]))
  #sf <- tibble(time = xf[[surv_vars[1]]],
  #             hazard = approxfun(x = haz$est.grid, y = haz$haz.est)(time))

  # TODO: refactor this. There is a lot of overlap with the dlr function.

  
  x_train <- make_model_matrix(formula, xf)
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
                use_bias = FALSE, activation = output_activation,
                kernel_initializer = "random_normal")

  type <- "hazard_dlr"
  y_train <- matrix(c(xf[[surv_vars[1]]], xf[[surv_vars[2]]]), ncol = 2)

  mm_col_names <- colnames(x_train)

  compile(model, loss = loss, optimizer = optimizer, metrics = metrics) 

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

  class(ret) <- c("survival_dglm", "dglm")
  ret
}

