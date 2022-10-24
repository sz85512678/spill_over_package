#' Compute the instrument ZW for (i) in Theorem 2 as a function of Q, offered,
#' and fraction_treated in group (excluding the current data point)
#' @importFrom MASS ginv
compute_ZW <- function(Q, offered, frac_treated_exclude_i){
  return(
    ginv(Q) %*%
    rbind(1, frac_treated_exclude_i, offered, offered * frac_treated_exclude_i)
  )
}

#' Compute the instrument Z1 for (ii) in Theorem 2 as a function of Q1, treated,
#' and fraction_treated in group (excluding the current data point)
#' @importFrom MASS ginv
compute_Z1_for_ii <- function(Q_1, treated, frac_treated_exclude_i){
  return(treated * ginv(Q_1) %*% rbind(1, frac_treated_exclude_i))
}

#' Compute the instrument Z1 for (iii) in Theorem 2 as a function of Q2, offered,
#' treated, and fraction_treated in group (excluding the current data point)
#' @importFrom MASS ginv
compute_Z1_for_iii <- function(Q_1, offered, treated, frac_treated_exclude_i){
  return(offered*(1-treated) * ginv(Q_1) %*% rbind(1, frac_treated_exclude_i))
}
