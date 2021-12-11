#Advent of Code Day 11

library(tidyverse)
library(here)
#Part One

input <- c("5483143223",
           "2745854711",
           "5264556173",
           "6141336146",
           "6357385478",
           "4167524645",
           "2176841721",
           "6882881134",
           "4846848554",
           "5283751526")

input_mat <- input %>% 
  map(str_split, "", simplify = TRUE) %>%
  map(as.integer) %>% 
  reduce(rbind) 

count_flashes <- function(x, iter = 100, part2 = FALSE) {
  
  
}