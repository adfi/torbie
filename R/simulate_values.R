#' Simulates values from a probability distribution
#'
#' Description here
#'
#' @param sample_size A integer value larger than zero
#' @param distribution A character value specifying the probability distribution
#' @param ... parameters of the probability distribution
#' @return A numeric vector
#' @examples
#' simulate_values(10, 'Normal')
#' simulate_values(100, 'Binomial', size=10, prob=0.5)
simulate_values <- function(sample_size, distribution, ...){
  switch(distribution,
         Normal=rnorm(sample_size, ...),
         Poisson=rpois(sample_size, ...),
         Binomial=rbinom(sample_size, ...),
         stop('Unknown distribution, only Normal, Poisson or Binomial are valid options'))
}
