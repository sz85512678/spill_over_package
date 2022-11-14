#' Compute parameters that identify the treatment effects
#'
#' @param data_in A data frame, containing columns "group_id", "offered",
#' "treated", "outcome"
#' @param val_s Vector of saturation levels
#' @param prob_s Vector of saturation assignment probabilities
#' @return A data frame with estimates and standard errors of various coeffcients
#' In particular
#'   \eqn{\gamma^e} is the population average average negative spill-over rate
#'   \eqn{\gamma^n} is the non-compilers average negative spill-over rate
#'   \eqn{\beta^c + \delta^c d_bar} is the average compiler direct effect,
#'   where d_bar is the avergae number of treated group neighbours.
#'   Various other treatment effects on the treated/untreated can be deduced
#'   from these coefficients, see Theorem 3 of the paper and its proof in the appendix.
#' @importFrom estimatr iv_robust tidy
#' @example
#' compute_treatment_effects(test_data_with_zero_sat, c(0, 0.25, 1), c(0.2, 0.5, 0.3))
#' @export
compute_treatment_effects <- function(data_in, val_s, prob_s){
  sat_all_positive = all(val_s > 0)
  if (sat_all_positive){
    data_out = compute_instruments(data_in, compute_moments_of_s(val_s, prob_s))
  }
  else{
    transformed_sat_designs = transform_saturations(val_s, prob_s)
    val_s = transformed_sat_designs[[1]]
    prob_s = transformed_sat_designs[[2]]
    data_out = compute_instruments_robust(data_in, compute_moments_of_s(val_s, prob_s))
  }

  data_out$ZW_1 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "cal_ZW"][[1]][1]
  ))
  data_out$ZW_2 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "cal_ZW"][[1]][2]
  ))
  data_out$ZW_3 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "cal_ZW"][[1]][3]
  ))
  data_out$ZW_4<- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "cal_ZW"][[1]][4]
  ))
  if (sat_all_positive == FALSE){
    data_out$ZW_5<- as.numeric(apply(
      data_out,
      1,
      function(x) x[names(data_out) == "cal_ZW"][[1]][5]
    )) # This instruments only exists for zero saturation cases.
  }
  data_out$Z_1_for_ii_1 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "Z_1_for_ii"][[1]][1]
  ))
  data_out$Z_1_for_ii_2 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "Z_1_for_ii"][[1]][2]
  ))
  data_out$Z_1_for_iii_1 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "Z_1_for_iii"][[1]][1]
  ))
  data_out$Z_1_for_iii_2 <- as.numeric(apply(
    data_out,
    1,
    function(x) x[names(data_out) == "Z_1_for_iii"][[1]][2]
  ))
  data_out$treated_frac_treated_exclude_i =
    data_out$frac_treated_exclude_i * data_out$treated

  ## Part (iv), the numbering is in terms of the paper appendix C. which is the opposite of Theorem 2
  if (sat_all_positive == FALSE){
    instruments <- paste("ZW_", 1:5, sep="")
  }
  else{
    instruments <- paste("ZW_", 1:4, sep="")
  }
  fmla <- as.formula(paste("outcome ~ frac_treated_exclude_i + treated + treated_frac_treated_exclude_i",
                           "| 0+", paste(instruments, collapse= "+")))
  theta_all = tidy(iv_robust(fmla, clusters = group_id, data = data_out, se_type = "stata"))
  theta_all$term = rbind("$\\alpha^e$","$\\gamma^e$", "$\\beta^c$","$\\delta^c$")

  ## Part (iii) -  Never takers: theta_n
  instruments <- paste("Z_1_for_iii_", 1:2, sep="")
  fmla <- as.formula(paste("outcome ~ frac_treated_exclude_i","| 0+",paste(instruments, collapse= "+")))
  theta_non_compliers = tidy(iv_robust(fmla, clusters = group_id, data = data_out, se_type = "stata"))
  theta_non_compliers$term = rbind("$\\alpha^n$","$\\gamma^n$")

  ## Part (ii) - Compliers: psi_c
  instruments <- paste("Z_1_for_ii_", 1:2, sep="")
  fmla <- as.formula(paste("outcome ~ frac_treated_exclude_i","| 0+",paste(instruments, collapse= "+")))
  psi_compilers = tidy(iv_robust(fmla, clusters = group_id, data = data_out, se_type = "stata"))
  psi_compilers$term = rbind("$\\alpha^c + \\beta^c$","$\\gamma^c + \\delta^c$")

  return(rbind(theta_all,  psi_compilers, theta_non_compliers))
}
