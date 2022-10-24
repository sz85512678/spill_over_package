library(plyr)
test_that("correctness of compute_moments_of_s", {
  val_s = c(0, 0.25, 1)
  prob_s =c(0.2, 0.5, 0.3)
  moments_of_s = compute_moments_of_s(val_s, prob_s)
  expect_equal(moments_of_s[2,1], 0.425)
  expect_equal(moments_of_s[2,2], 0.09375)
})
