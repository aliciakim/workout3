#' @title Binomial Cumulative
#' @description computes probability distribution and cumulative probabilites for set amount of trials
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return bindis a dataframe with columns for successes, probability, cumulative
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_cumulative(trials=5, prob=0.5)
#'
#' bin_cumulative(trials=3, prob =0.3)

bin_cumulative <- function(trials,prob){

  probabilities = bin_probability(trials = trials, prob = prob, success = 0:trials)
  binc <- data.frame(successes=0:trials, probability = probabilities, cumulative = cumsum(probabilities), stringsAsFactors = FALSE )

  class(binc) <- c("bincum", "data.frame")

  return(binc)

}

#' @export
plot.bincum <- function(z){
  plot(x=z$successes, y = z$cumulative, main = "Cumulative Distribution", xlab= "successes", ylab="probability", type = "o",ylim = c(0,1.1))

}
