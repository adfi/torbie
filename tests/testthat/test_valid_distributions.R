library(torbie)
context("Valid distributions")

test_that("a result is given when the correct distributions is given", {
    expect_true(length(simulate_values(10, "Normal")) == 10)
    expect_true(length(simulate_values(10, "Poisson", lambda = 1)) == 10)
    expect_true(length(simulate_values(10, "Binomial", size = 10, prob = 0.5)) == 10)
})

test_that("an error is thrown when an unknown distribution is given", {
    expect_error(simulate_values(10, "Gamma"))
    expect_error(simulate_values(10, "Cauchy"))
    expect_error(simulate_values(10, "Catastrophe"))
}) 
