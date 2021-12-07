#Advent of Code Day 5

library(tidyverse)
library(here)

#Part One

test_input <- c(3,4,3,1,2)
input <- scan(here("six/input.txt"), sep = ",")

for(i in seq_len(80)) {
  
  input <- input - 1
  
  if(any(input < 0)) {
    
    input <- c(input, rep(8, sum(input < 0)))
    
    input[input < 0] <- 6
    
  }
  
}
length(input)

#Part 2
start <- c(0, table(input), 0, 0, 0)

m <- matrix(0, nrow = 9, ncol = 9)
m[cbind(2:9, 1:8)] <- 1
m[1, c(7,9)] <- 1

reduce(1:256, ~ . %*% m, .init = start) %>% 
  sum()
