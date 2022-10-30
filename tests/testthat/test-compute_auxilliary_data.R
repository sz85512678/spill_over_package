test_that("correctness of compute_auxilliary_variables", {
  # Test the function on a dataset with non-zero saturations.
  processed_data = compute_auxilliary_data(test_data)
  expect_equal(processed_data$outcome, test_data$outcome)
  expect_equal(processed_data$frac_treated_exclude_i[2], 2/3, tolerance=1e-8)
  expect_equal(processed_data$group_size[5], 2)
  expect_equal(processed_data$frac_compilers_exclude_i[3], 1)

  # Test the function on a dataset with zero saturations
  processed_data_with_zero_sat = compute_auxilliary_data(test_data_with_zero_sat)
  expect_equal(processed_data_with_zero_sat$outcome, test_data_with_zero_sat$outcome)
  expect_equal(processed_data_with_zero_sat$frac_treated_exclude_i[2], 2/3, tolerance=1e-8)
  expect_equal(processed_data_with_zero_sat$group_size[5], 2)
  expect_equal(processed_data_with_zero_sat$frac_compilers_exclude_i[3], 1)

  expect_equal(processed_data_with_zero_sat$frac_compilers_exclude_i[7], NaN)
  expect_equal(processed_data_with_zero_sat$frac_compilers_exclude_i[9], NaN)
})
