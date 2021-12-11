#Advent of Code Day 11

library(tidyverse)
library(OpenImageR)
library(here)
#Part One

input <- read_lines(here("eleven/input.txt"))

input_mat <- input %>% 
  map(str_split, "", simplify = TRUE) %>%
  map(as.integer) %>% 
  reduce(rbind) 

kernel <- matrix(1:9 != 5, 3, 3)

count_flashes(input_mat, iter = 750, part2 = TRUE)

count_flashes <- function(x, iter = 100, part2 = FALSE) {
  
  count <- 0
  for(i in 1:iter) {
    
    x <- step1(x)
    nflashes <- sum(x == 0)
    if(part2 & nflashes == prod(dim(x)))
      return(i)
    count <- count + nflashes
    
  }
  if(part2)
    warning('No synchronization detected in ', iter, ' steps')
  count

}

step1 <- function(x) {
  x <- x + 1
  flashing <- flashed <- x == 10
  while (any(flashing)) {
    x <- x + OpenImageR::convolution(flashing, kernel)
    flashing <- x > 9 & !flashed
    flashed <- flashing | flashed
  }
  x[x > 9] <- 0
  x
}

add_neighbours <- function(x) {
  I <- nrow(x)
  J <- ncol(x)
  cbind(x[, -1], 0) +  # Right
    rbind(x[-1, ], 0) +  # Down
    cbind(0, x[, -J]) +  # Left
    rbind(0, x[-I, ]) +  # Up
    rbind(cbind(x[-1, -1], 0), 0) + # SE
    rbind(0, cbind(x[-I, -1], 0)) + # NE
    rbind(cbind(0, x[-1, -J]), 0) + # SW
    rbind(0, cbind(0, x[-I, -J]))   # NW
}
