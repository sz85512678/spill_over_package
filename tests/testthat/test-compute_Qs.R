test_that("computations of Q0, Q1, Q work", {
  moments_of_s = rbind(
    cbind(0.1, 0.2, 0.3, 0.4),
    cbind(0.2, 0.4, 0.2, 0.4),
    cbind(0.3, 0.3, 0.3, 0.3),
    cbind(0.9, 0.8, 0.7, 0.6)
  )
  frac_comp = 0.8
  group_size = 11

  Q_0 = compute_Q_0(moments_of_s, frac_comp, group_size)
  Q_1 = compute_Q_1(moments_of_s, frac_comp, group_size)
  Q = compute_Q(Q_0, Q_1)

  expect_equal(dim(Q_0), c(2,2))
  expect_equal(dim(Q_1), c(2,2))
  expect_equal(dim(Q), c(4,4))

  expect_equal(Q_0[1,1], 0.2)
  expect_equal(Q_0[2,2], 0.208)
  expect_equal(Q_1[1,2], 0.24)
  expect_equal(Q[2,1], 0.56)
  expect_equal(Q[2,3], 0.24)
})
