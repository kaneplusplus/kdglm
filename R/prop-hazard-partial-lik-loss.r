
group_sum <- function(elem) {
#  a <- k_cast(k_reshape(k_sum(elem), c(1, 1) ), k_floatx())
#  a <- tf$RaggedTensor$from_tensor(k_reshape(k_sum(elem), c(1, 1) ) )

#  browser()
#  nr <- k_constant(k_int_shape(elem)[1])
#  ret <- tf$cond( 
#    k_equal(nr, 1.),
#      function() {
#        a
#      },
#      function() {
#        k_concatenate(
#            list(a, k_zeros(c(k_shape(elem)[1] - 1L, 1L))), 1)
#      })
      

#  print(ret)
  tf$RaggedTensor$from_tensor(k_repeat_elements(k_reshape(k_sum(elem), c(1L, 1L)), 
    k_shape(elem)[1], 0))

}

tf_identity <- function(elem) {
  k_flatten(elem)
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
  thetas <- k_exp(sorted_preds) #k_exp(k_cast(sorted_preds, "float64"))
  theta_sum <- k_cumsum(thetas)
  sorted_status <- k_reshape(sorted_status, c(k_shape(sorted_status)[1], 1L))
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
