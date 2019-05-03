
context("Testing bin_probability")

test_that("bin_probability calculates correct probabilities", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_equal(bin_probability(success =55, trials = 100, prob = 0.45), 0.01075277)


})

test_that("bin_probability handles incorrect valuables", {
  expect_error(bin_probability(success = 5, trials = 1, prob = 0.5))
  expect_error(bin_probability(success = 2, trials = -5, prob = 0.5))
  expect_error(bin_probability(success = 2, trials = 5, prob = 1.3))
})



