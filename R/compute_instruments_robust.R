#' Compute instruments needed for identification, tolerate zero saturation designs
#'
#' Compute instruments needed for identification of treatment effects
#' with spill-over effect and
#' randomised saturation experiment and partial_compliance.
#' Assume each non-zero saturation group have at least two people offered treatment
#' and the data does not contain NA.
#'
#' @param data_in A data frame, containing columns "group_id", "offered",
#' "treated", "outcome",group_size", "frac_treated_exclude_i",
#' "frac_compilers_exclude_i".
#' @param moments_of_s A matrix, containing parameters related to the
#' randomised saturation experimental design, computed by the function
#' compute_moments_of_s
#' @return A data frame with containing the original data and additional columns
#'   "cal_ZW" containing 5/4 by 1 matrices, 5 if contains data-contains zero saturations
#'   "cal_Z1_for_ii" containing 2 by 1 matrices
#'   "cal_Z1_for_iii" containing 2 by 1 matrices
compute_instruments_robust <- function(data_in, moments_of_s){
  stopifnot("data should not have NA" = any(is.na(data_in)) == 0)
  data_in = compute_auxilliary_data(data_in)
  data_in$Q_0 <- apply(
    data_in,
    1,
    function(x) compute_Q_0_robust(
      moments_of_s,
      x[names(data_in) == "frac_compilers_exclude_i"][[1]],
      x[names(data_in) == "group_size"][[1]],
      x[names(data_in) == "group_total_offered"][[1]]
    ),
    simplify = FALSE
  )

  data_in$Q_1 <- apply(
    data_in,
    1,
    function(x) compute_Q_1_robust(
      moments_of_s,
      x[names(data_in) == "frac_compilers_exclude_i"][[1]],
      x[names(data_in) == "group_size"][[1]],
      x[names(data_in) == "group_total_offered"][[1]]
    ),
    simplify = FALSE
  )

  data_in$Q <- apply(
    data_in,
    1,
    function(x)
      compute_Q_robust(
        x[names(data_in) == "Q_0"][[1]],
        x[names(data_in) == "Q_1"][[1]],
        x[names(data_in) == "group_total_offered"][[1]]
      ),
    simplify = FALSE
  )

  data_in$cal_ZW <- apply(
    data_in,
    1,
    function(x) compute_ZW_robust(
      x[names(data_in) == "Q"][[1]],
      x[names(data_in) == "offered"][[1]],
      x[names(data_in) == "frac_treated_exclude_i"][[1]],
      x[names(data_in) == "group_total_offered"][[1]]
    ),
    simplify = FALSE
  )

  data_in$Z_1_for_ii <- apply(
    data_in,
    1,
    function(x) compute_Z1_for_ii_robust(
      x[names(data_in) == "Q_1"][[1]],
      x[names(data_in) == "treated"][[1]],
      x[names(data_in) == "frac_treated_exclude_i"][[1]],
      x[names(data_in) == "group_total_offered"][[1]]
    ),
    simplify = FALSE
  )

  data_in$Z_1_for_iii <- apply(
    data_in,
    1,
    function(x) compute_Z1_for_iii_robust(
      x[names(data_in) == "Q_1"][[1]],
      x[names(data_in) == "offered"][[1]],
      x[names(data_in) == "treated"][[1]],
      x[names(data_in) == "frac_treated_exclude_i"][[1]],
      x[names(data_in) == "group_total_offered"][[1]]
    ),
    simplify = FALSE
  )

  data_out = data_in
  return(data_out)
}
