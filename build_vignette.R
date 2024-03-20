
rm(list=ls())

## put in where the vignette is on you computer
wd <- "/Users/joelpick/github/squid_vignette"


## compile
bookdown::render_book(wd, bookdown::gitbook(split_by = "section+number"),new_session = TRUE)

# update.packages(ask = FALSE, checkBuilt = TRUE) 

# setwd("/Users/joelpick/github/squid_vignette")
# bookdown::render_book(input="02-sim_pop.Rmd",bookdown::gitbook(split_by = "section+number"),preview=TRUE,new_session = TRUE)

# devtools::install_version("stringr", version = "1.4.1")
# devtools::install_version("Matrix", version = "1.6.1")

#library(stringr)

# install.packages(c("Matrix","lme4"))
