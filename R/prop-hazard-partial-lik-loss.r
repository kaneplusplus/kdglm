
group_sum <- function(elem) {
  ret <- k_variable(k_zeros(k_int_shape(elem)[[1]]))
  k_set_value(ret[1], k_sum(elem))
  tf$RaggedTensor$from_tensor(k_reshape(k_eval(ret), c(length(ret), 1)))
}

make_ordering <- function(vals) {
  
}

#' @importFrom keras k_sum k_log k_map_fn
#' @importFrom tensorflow tf
neg_log_prop_haz_lik <- function(y_true, y_pred) {
  y_time <- y_true[, 1]
  y_status <- y_true[, 2]
  nr <- k_shape(y_true)[1]
  sv <- tf$math$top_k(y_time, nr, TRUE)
  sorted_time <- sv[0]
  sorted_indices <- sv[1]
  sorted_status <- k_gather(y_status, sorted_indices)
  sorted_preds <- k_gather(y_pred, sorted_indices)
  thetas <- k_exp(sorted_preds)

  spl <- tf$unique(sorted_time)[1]
  fixed_theta_sums <- 
    tf$map_fn(group_sum, 
            elems = tf$RaggedTensor$from_value_rowids(thetas, spl),
            fn_output_signature=tf$RaggedTensorSpec(shape=NULL, 
              ragged_rank = 1L, dtype = "float64"))
  fixed_theta_sums <- unlist(tf$RaggedTensor$to_list(fixed_theta_sums))

# The following was on the internet and it is not right.
#  theta_sum <- k_cumsum(thetas)
  theta_sum <- k_cumsum(fixed_theta_sums)
  print(theta_sum)
  -k_sum(k_cast(sorted_status, k_dtype(sorted_preds)) * sorted_preds - k_log(theta_sum))
}

neg_log_prop_haz_lik_ref <- function(y_true, y_pred) {
  y_time <- y_true[,1]
  y_status <- y_true[,2]
  sum_thetas <- vapply(y_time,
    function(t) {
      sum(exp(y_pred[ t <= y_time ]))
    },
    NA_real_)
  print(sort(sum_thetas))
  preds <- y_pred * y_status
  -sum(preds - log(sum_thetas))
}
