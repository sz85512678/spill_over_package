test_that("correctness of compute_auxilliary_variables", {
  load("test_data.RData")
  processed_data = compute_auxilliary_data(test_data)

  expect_equal(processed_data$outcome, test_data$outcome)
  expect_equal(processed_data$frac_treated_exclude_i[2], 2/3, tolerance=1e-8)
  expect_equal(processed_data$group_size[5], 2)
  expect_equal(processed_data$frac_compilers_exclude_i[3], 1)
})
