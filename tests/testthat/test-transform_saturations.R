test_that("transform saturation works", {
  val_s = c(0, 0.25, 1)
  prob_s =c(0.2, 0.5, 0.3)
  transformed_design = transform_saturations(val_s, prob_s)
  expect_equal(transformed_design[[1]][1], 0.25)
  expect_equal(transformed_design[[1]][2], 1)
  expect_equal(transformed_design[[2]][1], 0.625)
  expect_equal(transformed_design[[2]][2], 0.375)
})
