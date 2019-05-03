#' @title Choose Combinations
#' @description computes the number of combinations in which k successes can occur in n trials
#' @param n the number of trials
#' @param k the number of successes
#' @return combinations an integer representing how many combos
#' are made with k successes in n trials
#'
#' @export
#' @examples
#' #2 success in 5 trials
#' bin_choose(n=5, k=2)
#'
#' #1,2, or 3 successes in 5 trials
#' bin_choose(n=5, k = 1:3)

bin_choose <- function(n,k){
  check_trials(n)
  check_success(n,k)

  combinations <- factorial(n)/((factorial(k) * factorial(n-k)))
  return(combinations)
}
