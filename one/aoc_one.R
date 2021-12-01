# Advent of Code Day 1

# Part 1 ------------------------------------------------------------------


library(here)
library(tidyverse)

input_one <- read_lines(here("one/input_one.txt")) %>% 
  as_tibble() %>% 
  rename(depth = value) %>% 
  mutate(depth = as.numeric(depth))

input_one %>% 
  mutate(change = ifelse(depth > lag(depth), "Increased", "Decreased")) %>%
  count(change)
  

# Part 2 ------------------------------------------------------------------

library(slider)

tibble(window_depth = slide_dbl(input_one, sum, .after = 2, .complete = TRUE)) %>%
  mutate(change = if_else(window_depth > lag(window_depth), "Increased", "Decreased")) %>% 
  count(change)
