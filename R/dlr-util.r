
create_input_and_hidden_layers <- 
  function(x_train, hidden_layers, hidden_layers_activation, use_bias, 
           model_name) {
  
  model <- keras_model_sequential(name = model_name)

  for (i in seq_along(hidden_layers)) {
    if (i == 1) {
      model %>% layer_dense(input_shape = ncol(x_train),
                            units = hidden_layers[i],
                            activation = hidden_layers_activation[i],
                            name = paste("hidden_layer", i, sep = "_"),
                            use_bias = use_bias[i])
    } else {
      model %>% layer_dense(units = hidden_layers[i],
                            activation = hidden_layers_activation[i],
                            name = paste("hidden_layer", i, sep = "_"),
                            use_bias = use_bias[i])
    }
  }
  model
}
