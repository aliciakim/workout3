
Overview
--------

\`"Binomial" is an R package that implements functions for calculating probabilities of a Binomial random variable, and related calculations such as probability distribution, the expected value, variance, etc.

-   `bin_distribution()` computes probability distribution for set amount of trials (of class `bindis`)
-   `bin_cumulative()` computes probability distribution and cumulative probabilites for set amount of trials (of class `bincum`)
-   `bin_variable()` creates an object of class `binvar` which can be passed into `summary.binvar()` to make an object of class `summary.binvar`. This object can be inspected with `summary()` methods. \*`plot()` creates plots of objects of either `bindis` (a probability distribution histogram) or `bincum` (a cumulative distribution line plot)

Motivation
----------

This package was developed for UC Berkeley's Statistics 133 Spring 2019 class in order to gain familiarity with package creation in R.

Installation
------------

Install the development version from GitHub via the package `"devtools"`:

``` r
# development version from GitHub:
#install.packages("devtools") 

# install "binomial" (without vignettes)

#devtools::install_github("aliciakim/binomial")

# install "binomial" (with vignettes)

#devtools::install_github("aliciakim/binomial", build_vignettes = TRUE)
```

Usage
-----

``` r
library(binomial)

bin_variable(trials=10, prob=0.5)
#> Binomial Variable
#> 
#> Parameters
#> - number of trials:  10 
#> - prob of success:  0.5

#ways 3 successes can occur in 10 trials
bin_choose(n=10, k=3)
#> [1] 120

#probability of getting 2 successes in 5 trials
bin_probability(success=2, trials=5, prob=0.5)
#> [1] 0.3125

#create bindis object
bind1 <- bin_distribution(trials=10, prob=0.5)

#create bincum object
binc1 <- bin_cumulative(trials=10, prob=0.5)

#create binv object and get summary
binv1 <- bin_variable(trials=10, prob=0.5)
binv1
#> Binomial Variable
#> 
#> Parameters
#> - number of trials:  10 
#> - prob of success:  0.5

binv1sum <- summary(binv1)
binv1sum
#> Summary Binomial 
#> 
#> Parameters
#> - number of trials:  10 
#> - prob of success:  0.5 
#> 
#> Measures 
#> - mean:  5 
#> - variance:  2.5 
#> - mode:  5 
#> - skewness:  0 
#> - kurtosis:  -0.2


#summary measures
bin_mean(trials=10, prob=0.5)
#> [1] 5
bin_variance(trials=10, prob=0.5)
#> [1] 2.5
bin_mode(trials=10,prob=0.5)
#> [1] 5
bin_skewness(trials=10,prob=0.5)
#> [1] 0
bin_kurtosis(trials=10,prob=0.5)
#> [1] -0.2
```
