# Advent of Code Day Three

library(tidyverse)
library(here)
#Part One

input <- read_lines(here("three/input.txt")) %>% 
  as_tibble() %>% 
  separate(col = value, into = letters[1:13], sep = "") %>% 
  select(-a)

rates <- input %>% 
  pivot_longer(cols = everything(), 
               names_to = "lets", 
               values_to = "binaries") %>% 
  group_by(lets) %>% 
  count(binaries) %>% 
  mutate(rate_type = min_rank(n),
         rate_type = ifelse(rate_type == 1, "epsilon", "gamma")) %>% 
  ungroup() %>% 
  select(binaries, rate_type) %>% 
  pivot_wider(names_from = rate_type, values_from = binaries) 

gamma_rate <- rates$gamma[[1]] %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)
epsilon_rate <- rates$epsilon[[1]] %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)

gamma_rate * epsilon_rate

# Part 2
input <- read_lines(here("three/input.txt"))

mat <- str_split(input, "") %>% 
  map(as.integer) %>% 
  do.call(rbind, .)

oxygen_rating <- vector("numeric", length = ncol(mat))

for(i in 1:ncol(mat)) {
  
  target <- round(mean(mat[, i]) + 1e-6)
  
  
  oxygen_rating[i] <- mat[mat[, i] == target, ]
  
}

filter_matrix <- function(m, index, most_common = TRUE) {
  
  target <- round(mean(m[, index]) + 1e-6)
  
  if(!most_common) {
    
    target <- 1-target
    
  }
  
  ret <- m[m[, index]==target, ]
  
  if (!is.matrix(ret)) {
    done(ret)
  }else {
    ret
  }
}

to_decimal <- function(x) {
  strtoi(paste(x, collapse = ""), base = 2)
}

oxygen <- reduce(1:12, filter_matrix, .init = mat)
co2 <- reduce(1:12, filter_matrix, .init = mat, most_common = FALSE)

to_decimal(oxygen) * to_decimal(co2)
