#' @title Binomial Variable
#' @description computes binvar attributes
#' @param trials the number of trials
#' @param prob number that is the probability of success
#' @return bindis a last with trials and prob
#'
#' @export
#' @examples
#' #5 trials with success prob at 0.5
#' bin_variable(trials=5, prob=0.5)
#'
#' lst <- bin_variable(trials=5, prob = 0.5)
#' print(lst)
#' summary(lst)

bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  binv <- list("trials" = trials,
               "prob" = prob)

  class(binv) <- "binvar"

  return(binv)

}


#' @export
print.binvar <- function(x,...){

  cat("Binomial Variable\n\n")
  cat("Parameters\n")
  cat("- number of trials: ", x$trials, "\n")
  cat("- prob of success: ", x$prob, "\n")
  invisible(x)

}


#' @export
summary.binvar <- function(x,...){

  xmean = aux_mean(x$trials, x$prob)
  xvariance = aux_variance(x$trials, x$prob)
  xmode = aux_mode(x$trials, x$prob)
  xskewness = aux_skewness(x$trials, x$prob)
  xkurtosis = aux_kurtosis(x$trials, x$prob)

  sumbin <- list("trials" = x$trials,
                 "prob" = x$prob,
                 "mean" = xmean,
                 "variance" = xvariance,
                 "mode" = xmode,
                 "skewness" = xskewness,
                 "kurtosis" = xkurtosis
                 )

  class(sumbin) <- "summary.binvar"
  return(sumbin)

}

#' @export
print.summary.binvar <- function(x, ...){
  cat("Summary Binomial \n\n")
  cat("Parameters\n")
  cat("- number of trials: ", x$trials, "\n")
  cat("- prob of success: ", x$prob, "\n\n")

  cat("Measures \n")
  cat("- mean: ", x$mean, "\n")
  cat("- variance: ", x$variance, "\n")
  cat("- mode: ", x$mode, "\n")
  cat("- skewness: ", x$skewness, "\n")
  cat("- kurtosis: ", x$kurtosis, "\n")

  invisible(x)
}
