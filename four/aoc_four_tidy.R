#Advent of Code Day Four

library(tidyverse)
library(here)

#Part One

input <- read_lines(here("four/input.txt"), na = "") %>% 
  as_tibble() %>% 
  rename(x = value)

draws <- as.integer(str_split(input$x[1], ",")[[1]])

boards <- input %>%
  slice(-1) %>% 
  mutate(board = cumsum(is.na(x))) %>% 
  filter(!is.na(x)) %>% 
  group_by(board) %>% 
  mutate(row = row_number()) %>% 
  separate_rows(x, sep = " +", convert = TRUE) %>% 
  filter(!is.na(x)) %>% 
  group_by(board, row) %>% 
  mutate(col = row_number()) %>% 
  ungroup() %>% 
  mutate(turn = match(x, draws))

board_turn_win <- boards %>% 
  gather(coordinate, value, row, col) %>%
  group_by(board, coordinate, value) %>% 
  summarize(complete_on_turn = max(turn)) %>%
  group_by(board) %>% 
  summarize(win_on_turn = min(complete_on_turn))

board_scores <- board_turn_win %>% 
  inner_join(boards, by = "board") %>% 
  group_by(board, win_on_turn) %>% 
  summarize(total_undrawn = sum(x[turn > win_on_turn]),
            .groups = "drop") %>% 
  mutate(answer = draws[win_on_turn] * total_undrawn)
  
board_scores %>% 
  slice_min(win_on_turn)

board_scores %>% 
  slice_max(win_on_turn)

cards %>% 
  mutate(card_num = cumsum(is.na(nums)))
