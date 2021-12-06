#Advent of Code Day 5

library(tidyverse)
library(here)

#Part One

test_input <- read_lines(here("five/test_input.txt")) %>% 
  as_tibble()

input <- read_lines(here("five/input.txt")) %>% 
  as_tibble()
  

first_input <- input %>% 
  separate(value, into = c("from", "to"), sep = " -> ") %>% 
  separate(from, into = c("from_x", "from_y"), sep = ",") %>% 
  separate(to, into = c("to_x", "to_y"), sep = ",") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(x = map2(from_x, to_x, seq),
         y = map2(from_y, to_y, seq))

horizontal_vertical <- first_input %>% 
  filter(from_x == to_x | from_y == to_y) %>% 
  unnest(x) %>% 
  unnest(y)

horizontal_vertical %>% 
  count(x, y) %>% 
  summarize(sum(n > 1))

vent_map <- matrix(data = 0, nrow = 1000, ncol = 1000)

for(i in 1:nrow(input_tibble)) {
  
  for(j in seq(input_tibble$from_y[i] + 1, input_tibble$to_y[i] + 1)) {
    
    for(k in seq(input_tibble$from_x[i] + 1, input_tibble$to_x[i] + 1)) {
      
      if(vent_map[j, k] == 0) {
        
        vent_map[j, k] <- 1
        
      }else{
        
        vent_map[j, k] <- vent_map[j, k] + 1
        
      }
    }
  }
}

vent_map %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), 
               names_to = "names", 
               values_to = "values") %>% 
  filter(values >= 2)


#Part 2

input_tibble <- input %>% 
  separate(value, into = c("from", "to"), sep = " -> ") %>% 
  separate(from, into = c("from_x", "from_y"), sep = ",") %>% 
  separate(to, into = c("to_x", "to_y"), sep = ",") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(x = map2(from_x, to_x, seq),
         y = map2(from_y, to_y, seq))

diagonal <- input_tibble %>% 
  filter(!(from_x == to_x | from_y == to_y)) %>% 
  unnest(c(x, y))

bind_rows(horizontal_vertical, diagonal) %>% 
  count(x, y) %>%
  summarize(sum(n >= 2))
