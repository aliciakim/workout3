context("Check the checkers")

test_that("check_prob with valid probabilities", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(2))
  expect_error(check_prob(-0.5))
  expect_error(check_prob(c(0.4, 0.5)))
  expect_true(check_prob(0))

})


test_that("check_trials with valid trials",{
  expect_true(check_trials(2))
  expect_error(check_trials(0))
  expect_error(check_trials(-4))
  expect_error(check_trials(-4.5))
  expect_error(check_trials(c(1,3,4)))
})


test_that("check_success with valid successes",{
  expect_true(check_success(3, c(1, 2, 3)))
  expect_error(check_success(3, c(1.5, 2)))
  expect_error(check_success(4, c(1,2,5)))
  expect_error(check_success(4, 5))
  expect_error(check_success(4, -1))

})
