#' @title Binomial Distribution
#' @description computes probability distribution for set amount of trials
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return bindis a dataframe with the probability distribution: successes in first column, probability in second
#'
#' @export
#' @examples
#' #2 success in 5 trials
#' bin_distribution(trials=5, success=2, prob = 0.5)
#'
#' bin_distribution(trials=3, succcess=1, prob =0.3)

bin_distribution <- function(trials,prob){

  probabilities = bin_probability(trials = trials, prob = prob, success = 0:trials)

  bind <- data.frame(successes=0:trials, probability = probabilities, stringsAsFactors = FALSE )

  class(bind) <- c("bindis", "data.frame")

  return(bind)

}

#' @export
plot.bindis <- function(x, ...){
  barplot(x$probability, names.arg = x$successes, xlab = "successes", ylab = "probability", ylim=c(0,1), main = "Probability Histogram")
  }

