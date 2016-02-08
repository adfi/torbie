#' Simulates values from a probability distribution
#'
#' Produces a \code{sample_size} number of values Using the random generation
#' function for the given \code{distribution}. Only the Normal, Binomial
#' or Poisson distribution are currently accepted.
#'
#' @export
#' @param sample_size A integer value larger than zero
#' @param distribution A character value specifying the probability distribution
#' @param ... parameters of the probability distribution
#' @return A numeric vector
#' @details
#' For the Normal distribution no parameters are necessary, this defaults to the
#' Standard Normal distribution. However, the Poisson and Binomial distribution do
#' require parameters and an error is thrown if these are not provided.
#' @seealso \code{\link{rnorm}}, \code{\link{rpois}}, \code{\link{rbinom}}
#' @examples
#' simulate_values(10, 'Normal')
#' simulate_values(100, 'Binomial', size=10, prob=0.5)
simulate_values <- function(sample_size, distribution, ...){
  if(sample_size < 0) {
    stop("Sample size must be larger than zero")
  }
  switch(distribution,
         Normal=rnorm(sample_size, ...),
         Poisson=rpois(sample_size, ...),
         Binomial=rbinom(sample_size, ...),
         stop('Unknown distribution, only Normal, Poisson or Binomial are valid options'))
}
