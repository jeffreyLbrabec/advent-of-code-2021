#Advent of code Day 7

library(tidyverse)
library(here)

#Part 1
input <- read_lines(here("seven/input.txt")) %>% 
  str_split(",") %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = as.integer(value))

input %>% 
  mutate(med_val = median(value)) %>% 
  mutate(fuel = abs(value - med_val)) %>% 
  summarize(fuel_total = sum(fuel))

#Part 2

input <- c(16,1,2,0,4,2,7,1,2,14) %>% as_tibble()

input_fuels <- input %>% 
  mutate(mean_val_c = ceiling(mean(value)),
         mean_val_f = floor(mean(value))) %>% 
  mutate(fuel_c = abs(value - mean_val_c),
         fuel_f = abs(value - mean_val_f))

min(sum(input_fuels$fuel_c + input_fuels$fuel_c*(input_fuels$fuel_c - 1)/2),
    sum(input_fuels$fuel_f + input_fuels$fuel_f*(input_fuels$fuel_f - 1)/2))
