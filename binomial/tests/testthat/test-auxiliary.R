context("Check the aux")

test_that("aux_mean computes means", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(100, 0.5), 50)
  expect_equal(aux_mean(10, 0.1), 1)

})



test_that("aux_variance computes variance", {
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_equal(aux_variance(5, 0.6), 1.2)
  expect_equal(aux_variance(100, 0.1), 9)

})


test_that("aux_mode computes mode", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(100, 0.5), 50)
  expect_equal(aux_mode(10, 0.1), 1)

})



test_that("aux_skewness computes skew", {
  expect_equal(aux_skewness(10, 0.3), 0.2760262, tolerance=.002)
  expect_equal(aux_skewness(35, 0.6), -0.06900656,tolerance=.002)
  expect_equal(aux_skewness(26, 0.33), 0.1418071,tolerance=.002)

})


test_that("aux_kurtosis computes kurtosis", {
  expect_equal(aux_kurtosis(10, 0.3), -0.1238095, tolerance=.002)
  expect_equal(aux_kurtosis(150, 0.6), -0.01222222, tolerance=.002)
  expect_equal(aux_kurtosis(54, 0.23), -0.006545789,tolerance=.002)

})
