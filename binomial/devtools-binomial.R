
#Devtools workflow
#library(devtools)

devtools::document() #generate documentation
devtools::check_man() #generate documentation
devtools::test() #run tests
devtools::build_vignettes() #build vignettes
devtools::build() #build bundle
devtools::install() #install package
devtools::check() #a comprehensive check (ensures CRAN upload-ability)
