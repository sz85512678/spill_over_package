#' Compute Q_0 according to the formula in Appedix C, step 3
compute_Q_0 <- function(moments_of_s, frac_compilers_exclude_i, group_size){
  return(
    cbind(
      rbind(
        moments_of_s[1,2],
        moments_of_s[2,2] * frac_compilers_exclude_i
      ),
      rbind(
        moments_of_s[2,2] * frac_compilers_exclude_i,
        moments_of_s[3,2] * frac_compilers_exclude_i^2
        + moments_of_s[2,3] * frac_compilers_exclude_i/(group_size - 1)
      )
    )
  )
}
#' Compute Q_1 according to the formula in Appedix C, step 3
compute_Q_1 <- function(moments_of_s, frac_compilers_exclude_i, group_size){
  return(
    cbind(
      rbind(
        moments_of_s[2,1],
        moments_of_s[3,1] * frac_compilers_exclude_i
      ),
      rbind(
        moments_of_s[3,1] * frac_compilers_exclude_i,
        moments_of_s[4,1] * frac_compilers_exclude_i^2
        + moments_of_s[3,2] * frac_compilers_exclude_i/(group_size - 1)
      )
    )
  )
}
#' Compute Q according to the formula in Appedix C, step 4
compute_Q <- function(Q_0, Q_1){
  return( cbind(rbind(Q_0 + Q_1, Q_1), rbind(Q_1, Q_1)) )
}
#' Compute Q_0 for possibly zero saturation, return NA if saturation = 0.
compute_Q_0_robust <- function(moments_of_s, frac_compilers_exclude_i,
                               group_size, total_offered){
  if (total_offered > 0){
    return(compute_Q_0(moments_of_s, frac_compilers_exclude_i, group_size))
  } else {
    return(NA)
  }
}
#' Compute Q_1 for possibly zero saturation, return NA if saturation = 0.
compute_Q_1_robust <- function(moments_of_s, frac_compilers_exclude_i,
                               group_size, total_offered){
  if (total_offered > 0){
    return(compute_Q_1(moments_of_s, frac_compilers_exclude_i, group_size))
  } else {
    return(NA)
  }
}
#' Compute Q for possibly zero saturation, return NA if saturation = 0.
compute_Q_robust <- function(Q_0, Q_1, total_offered){
  if(total_offered > 0){
    return( cbind(rbind(Q_0 + Q_1, Q_1), rbind(Q_1, Q_1)) )
  } else {
    return(NA)
  }
}


