## requires Rd2roxygen 
library("Rd2roxygen"))

## remove the man directory first
unlink("man", recursive = TRUE)

## go up a level
owd = setwd("..")

library(Rd2roxygen)
options(width = 75)

## run roxygen and several cleaning up steps
try(rab("tourrGui", "tourrGui", install = TRUE, copy.package = FALSE))

setwd(owd)
