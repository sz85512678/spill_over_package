#' Compute conditioning (on saturation being non-zero) probabilities of being assigned a saturation level.
#'
#' Given the design of a randomised saturation experiment with a zero saturation
#' group, compute saturation values and probabilities conditioning on the
#' saturation being non-zero.
#' @param val_s, values of original saturation levels.
#' @param prob_s, probability of being assigned each original saturation levels.
#' @return list(val_s_excl_zero, prob_s_excl_zero), transformed saturation levels
#' and probabilities.
#' @export
transform_saturations <- function(val_s, prob_s){
  stopifnot("The original saturation vals are assumed to contain a zero" =
              any(val_s == 0))
  val_s_excl_zero <- val_s[- match(0, val_s)]
  prob_s_excl_zero <- prob_s[- match(0, val_s)]
  prob_s_excl_zero <- prob_s_excl_zero/sum(prob_s_excl_zero)
  return(list(val_s_excl_zero, prob_s_excl_zero))
}
