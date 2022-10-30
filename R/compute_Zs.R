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

#' Compute ZW for (i) in Theorem 2, for designs with zero-saturation groups,
#' using the formula in Appendix D.
compute_ZW_robust <- function(Q, offered, frac_treated_exclude_i, total_offered){
  if (total_offered > 0){
    return( rbind(compute_ZW(Q, offered, frac_treated_exclude_i), 0) )
  } else{
    return( rbind(0, 0, 0, 0, 1))
  }
}
#' Compute Z1 for (ii) for designs with zero-saturation groups,
#' if saturation is zero return NA.
compute_Z1_for_ii_robust <- function(Q_1, treated, frac_treated_exclude_i, total_offered){
  if (total_offered > 0){
    return(compute_Z1_for_ii(Q_1, treated, frac_treated_exclude_i))
  } else {
    return(NA)
  }
}
#' Compute Z1 for (iii) for designs with zero-saturation groups,
#' if saturation is zero return NA
compute_Z1_for_iii_robust <- function(Q_1, offered, treated,
                                      frac_treated_exclude_i, total_offered){
  if (total_offered > 0){
    return(compute_Z1_for_iii(Q_1, offered, treated, frac_treated_exclude_i))
  } else {
    return(NA)
  }
}
