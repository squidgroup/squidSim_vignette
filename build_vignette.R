
rm(list=ls())

## put in where the vignette is on you computer
wd <- "/Users/joelpick/github/squid_vignette"

## compile
bookdown::render_book(wd, bookdown::gitbook(split_by = "section+number"))

