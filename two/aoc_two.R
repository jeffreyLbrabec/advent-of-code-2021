#Advent of Code Day Two
library(tidyverse)
library(here)

## Part 1
input <- read_lines(here("two/input.txt")) %>% 
  as_tibble() %>% 
  separate(col = value, into = c("direction", "distance"), sep = " ") %>% 
  mutate(distance = as.numeric(distance))


forward_dist <- input %>% 
  filter(direction == "forward") %>% 
  summarize(forward_dist = sum(distance))

horizontal_dist <- input %>% 
  filter(direction == "down" | direction == "up") %>% 
  mutate(new_distance = case_when(direction == "up" ~ distance*-1,
                                  direction == "down" ~ distance*1)) %>% 
  summarize(horiz_dist = sum(new_distance))

forward_dist$forward_dist * horizontal_dist$horiz_dist

## Part 2

input %>% 
  mutate(aim = case_when(
    direction == "up" ~ distance * -1, 
    direction == "down" ~ distance,
    TRUE ~ 0),
    current_aim = cumsum(aim),
    plane = ifelse(direction == "forward", "x", "y"),
    x = ifelse(direction == "forward", distance, 0),
    change_depth = ifelse(direction == "forward", distance * current_aim, 0)) %>% 
  summarize(depth = sum(change_depth),
            horizontal = sum(x),
            answer = depth * horizontal) %>% 
  pull(answer)