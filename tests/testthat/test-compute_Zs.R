test_that("computing Zs works", {
  Q = rbind( cbind(1, 0, 0, 0),
             cbind(0, 1, -1, 0),
             cbind(0, 0, 1, 0),
             cbind(0, 0, 0, 1) )
  offered = 1
  frac_treated_exclude_i = 0.4
  expect_equal(compute_ZW(Q, offered, frac_treated_exclude_i),
               rbind(1, 1.4, 1, 0.4))

  Q1 = rbind( cbind(1, 2),
              cbind(3, 4))
  treated = 1
  expect_equal(compute_Z1_for_ii(Q1, treated, frac_treated_exclude_i),
               rbind(-1.6, 1.3))

  treated = 0
  offered = 1
  frac_treated_exclude_i = 0.2
  expect_equal(compute_Z1_for_iii(Q1, offered, treated, frac_treated_exclude_i),
               rbind(-1.8, 1.4))
})
