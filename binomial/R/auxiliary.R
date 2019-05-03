# private function that returns value of mean
# given trials and prob
# (i.e. np)
aux_mean <- function(trials, prob){
  return(trials * prob)
}


# private function that returns value of variance
# given trials and prob
# (i.e. np(1-p))
aux_variance <- function(trials, prob){
  return((trials * prob) * (1-prob))
}


# private function that returns value of std
# given trials and prob
# (i.e. sqrt(variance))
aux_std <- function(trials, prob){
  return(sqrt((trials * prob) * (1-prob)))
}


# private function that returns value of mode
# given trials and prob
# (i.e. int(np+p))
aux_mode <- function(trials, prob){
  return(as.integer(trials*prob + prob))
}


# private function that returns value of skewness
# given trials and prob
# (i.e. (1-2p)/(sqrt(np(1-p))))
aux_skewness <- function(trials, prob){
  return((1-(2*prob))/ sqrt( (trials*prob)*(1-prob)))
}

# private function that returns value of kurtosis
# given trials and prob
# (i.e. ((1-6p)(1-p))/(np(1-p)))
aux_kurtosis <- function(trials, prob){
  return( (1- ((6*prob) * (1-prob))) / ((trials*prob) * (1-prob)))
}



