#' @title Binomial Probability
#' @description computes probability of getting k successes in n trials with specified probability of success
#' @param trials the number of trials
#' @param success the number of successes
#' @param prob number that is the probability of success
#' @return probability a number representing probability of getting success successes in trials trials with probability prob
#'
#' @export
#' @examples
#' #2 success in 5 trials
#' bin_choose(trials=5, success=2, prob = 0.5)
#'
#' #1,2, or 3 successes in 5 trials
#' bin_choose(trials=5, success = 0:2, prob = 0.5)

bin_probability <- function(trials,success,prob){
  check_trials(trials)
  check_success(trials,success)
  check_prob(prob)

  combinations <- bin_choose(trials,success)
  successes <- prob ** success
  failures <- (1-prob) ** (trials-success)
  probability <- combinations * successes * failures
  return(probability)
}
