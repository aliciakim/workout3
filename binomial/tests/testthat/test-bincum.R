context("Testing bin_cumulative")

test_that("bin_cumulative calculates correct distribution dataframe",{

  #constructing dataframe from example
  probabilities <- bin_probability(trials = 5, success = 0:5, prob = 0.5)
  dat <- data.frame(successes = 0:5, probability = probabilities, cumulative = cumsum(probabilities), stringsAsFactors = FALSE)
  class(dat) <- c("bincum", "data.frame")

  expect_identical(dat, bin_cumulative(5, 0.5))
  expect_equal(bin_cumulative(5, 0.5)$successes, c(0:5))
  expect_equal(bin_cumulative(5, 0.5)$probability, c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  expect_equal(sum(bin_cumulative(5, 0.5)$probability), 1)

  #plotting binomial cumultiave distribution
  plot(dat)

  #constructing dataframe of length 1
  probabilities <- bin_probability(trials = 1, success = 0:1, prob = 0.25)
  dat <- data.frame(successes = 0:1, probability = probabilities, cumulative = cumsum(probabilities), stringsAsFactors = FALSE)
  class(dat) <- c("bincum", "data.frame")

  expect_identical(dat, bin_cumulative(trials=1, prob = 0.25))
  expect_equal(sum(bin_cumulative(1, 0.25)$probability), 1)

  #plotting binomial cumultiave distribution
  plot(dat)

})
