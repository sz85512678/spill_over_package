# These three functions compute Qs, as a step towards computing the instruments
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

compute_Q <- function(Q_0, Q_1){
  return( cbind(rbind(Q_0 + Q_1, Q_1), rbind(Q_1, Q_1)) )
}
