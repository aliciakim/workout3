#' @title Binomial Mean
#' @description computes mean given trials and probs
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return mean the numerical mean
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_mean(trials=5, prob=0.5)


bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)


  return(aux_mean(trials, prob))

}


#' @title Binomial Variance
#' @description computes variance given trials and probs
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return variance, the numerical variance
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_variance(trials=5, prob=0.5)


bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)


  return(aux_variance(trials, prob))

}


#' @title Binomial mode
#' @description computes mode given trials and probs
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return mode, the numerical mean
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_mean(trials=5, prob=0.5)


bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)


  return(aux_mode(trials, prob))

}



#' @title Binomial Skewness
#' @description computes skewness given trials and probs
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return skewness the numerical mean
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_skewness(trials=5, prob=0.5)


bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)


  return(aux_skewness(trials, prob))

}


#' @title Binomial Kurtosis
#' @description computes kurtosis given trials and probs
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return kurtosis the numerical mean
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_kurtosis(trials=5, prob=0.5)


bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)


  return(aux_kurtosis(trials, prob))

}
