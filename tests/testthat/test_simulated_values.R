library(torbie)
context("Simulated values")

test_that("simulated values are returned for the Normal distribution",{
  set.seed(31459)
  expect_equal(simulate_values(5, 'Normal'),
               c(-1.5737368,-1.4417534,0.4186893,-0.2627846,0.6046170),
               tolerance=1e-05)
  expect_equal(simulate_values(5, 'Normal', mean=10),
               c(11.86305,10.72662,11.16738,10.19025,10.10490),
               tolerance=1e-05)
  expect_equal(simulate_values(5, 'Normal', mean=5, sd=5),
               c(7.47626248,8.67423014,14.94870594,0.88168120,-0.03901771),
               tolerance=1e-05)
})

test_that("simulated values are returned for the Poisson distribution",{
  set.seed(31459)
  expect_equal(simulate_values(5, 'Poisson', lambda=10),
               c(16,14,11,7,12))
  expect_error(simulate_values(5, 'Poisson'))
})

test_that("simulated values are returned for the Binomial distribution",{
  set.seed(31459)
  expect_equal(simulate_values(5, 'Binomial', size=10, prob=0.5),
               c(3,5,3,5,6))
  expect_error(simulate_values(5, 'Binomial'))
})
