test_that("compute instruments robust works", {
  data_out = compute_instruments_robust(test_data_with_zero_sat, moments_of_s)
  expect_equal(
    "cal_ZW" %in% names(data_out) &
      "Z_1_for_ii" %in% names(data_out) &
      "Z_1_for_iii" %in% names(data_out),
    TRUE
  )
  expect_equal(data_out$cal_ZW[[1]], rbind(0, 0, 8.405333, -7.765333, 0), tolerance = 1e-4)
  expect_equal(data_out$Z_1_for_ii[[1]], rbind(8.4053, -7.7653), tolerance = 1e-4)
  expect_equal(data_out$Z_1_for_iii[[5]], rbind(0, -0), tolerance = 1e-4)
  expect_equal(data_out$cal_ZW[[8]], rbind(0, 0, 0, -0, 1), tolerance = 1e-4)
})
