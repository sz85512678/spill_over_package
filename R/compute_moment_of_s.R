#' Compute expectations of [s^(i-1)][(1-s)^(j-1)]
#'
#' Compute expectations of [s^(i-1)][(1-s)^(j-1)]
#' for (i,j) = (1,2), (2,1), (2,2), (2,3), (3,1), (3,2), (4,1)
#' @param val_s The set of values of s
#' @param prob_s The probability of s equals to corresponding entries in val_s
#' @return moments_of_s, a matrix of doubles whose (i,j) entry stores E[s^(i-1)][(1-s)^(j-1)]
#' @export
compute_moments_of_s <- function(val_s, prob_s)
{
  stopifnot(length(val_s) == length(prob_s))
  moments_of_s = matrix(NA, nrow=4, ncol=4)
  for (v in list(c(1,2), c(2,1), c(2,2), c(2,3), c(3,1), c(3,2), c(4,1))){
    i = v[1]
    j = v[2]
    moments_of_s[i, j] <- weighted.mean(val_s^(i - 1) * (1 - val_s)^(j - 1), prob_s)
  }
  return(moments_of_s)
}
