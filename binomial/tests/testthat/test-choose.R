
context("Testing bin_choose")

test_that("bin_choose calculates choose", {
  expect_equal(bin_choose(5,seq(4, 5)), c(5, 1))
  expect_equal(bin_choose(5, 3), 10)
  expect_equal(bin_choose(5, 0), 1)
  expect_equal(bin_choose(1, 0), 1)

})

test_that("bin_choose handles incorrect valuables", {
  expect_error(bin_choose(5, 6))
  expect_error(bin_choose(6, seq(4, 7)))
  expect_error(bin_choose(0, 1))
  expect_error(bin_choose(5, seq(1,6)))
})



