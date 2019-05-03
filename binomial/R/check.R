# private function to check that numeric p is a valid probability value
# (i.e. 0 <= p <= 1)
check_prob <- function(p){
  if(length(p)>1){
    stop("p must be of length 1")
  }
  if(!is.numeric(p)){
    stop("p has to be a number between 0 and 1!")
  }


  if((p < 0) | (p>1)){
    stop("p has to be a number between 0 and 1!")
  }
  TRUE
}


# private function to check that integer n is a valid value for number of trials
# (i.e. n >= 0)
check_trials <- function(n){
  if(length(n)>1){
    stop("trials must be of length 1")
  }
  if(!(is.numeric(n))){
    stop("invalid trials value: must be an integer")
  }

  if ((n %% 1) != 0) {
    stop('n must be a non-negative integer')
  }

  if(n<1){
    stop("invalid trials value: must be non-negative value")
  }
  TRUE
}


# private function to check that success vector has valid number of successes
# (i.e. 0 <= success <= n)
check_success <- function(trials, success){
  if( (!(is.numeric(success)) | (any(is.na(success))) | any(success < 0))){
    stop("invalid success value: must be vector of non-negative integers")
  }

  if (any((success %% 1) != 0)) {
    stop('success must be a vector of non-negative integer')
  }

  if(any(success > trials)){
    stop("success cannot be greater than trials")
  }
  TRUE
}
