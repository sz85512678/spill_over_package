test_that("treatment effect computation returns no syntax error", {
  compute_treatment_effects(test_data_with_zero_sat, c(0, 0.25, 1), c(0.2, 0.5, 0.3))
  expect_equal(0, 0)
})
