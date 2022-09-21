
rm(list=ls())

## put in where the vignette is on you computer
wd <- "/Users/joelpick/github/squid_vignette"


## compile
bookdown::render_book(wd, bookdown::gitbook(split_by = "section+number"),new_session = TRUE)

# setwd("/Users/joelpick/github/squid_vignette")
# bookdown::render_book(input="02-sim_pop.Rmd",bookdown::gitbook(split_by = "section+number"),preview=TRUE,new_session = TRUE)
