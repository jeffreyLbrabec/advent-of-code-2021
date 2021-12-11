#Advent of Code Day nine

library(tidyverse)
library(slider)
library(here)

#Part one

input <- read_lines(here("nine/input.txt")) %>% 
  as_tibble() %>% 
  separate(value, into = as.character(c(1:101)), sep = "") %>% 
  dplyr::select(-`1`) %>% 
  mutate(across(everything(), as.integer))



mat_input <- input %>% 
  as.matrix()

pos_map <- matrix(data = ".", nrow = 100, ncol = 100)

for(i in 1:nrow(mat_input)) {
  
  for(j in 1:ncol(mat_input)) {
    
    if(i == 1 & j == 1) {
      bottom <- mat_input[i,j] < mat_input[i + 1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      
      all_dirs <- c(bottom, right)
    }else if(i == 1 & j == 100) {
      
      bottom <- mat_input[i,j] < mat_input[i + 1, j]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(bottom, left)
    }else if(i == 1 & j != 1 & j != 100) {
      
      bottom <- mat_input[i,j] < mat_input[i + 1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(bottom, right, left)
    }else if(i == 100 & j == 1) {
      
      top <- mat_input[i,j] < mat_input[i-1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      
      all_dirs <- c(top, right)
    }else if(i == 100 & j == 100) {
      
      top <- mat_input[i,j] < mat_input[i-1, j]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(top, left)
    }else if(i == 100 & j != 1 & j != 100) {
      
      top <- mat_input[i,j] < mat_input[i-1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(top, right, left)
    }else if(j == 1 & i != 1 & i != 100) {
      
      bottom <- mat_input[i,j] < mat_input[i + 1, j] 
      top <- mat_input[i,j] < mat_input[i-1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      
      all_dirs <- c(bottom, top, right)
    }else if(j == 100 & i != 1 & i != 100) {
      
      bottom <- mat_input[i,j] < mat_input[i + 1, j] 
      top <- mat_input[i,j] < mat_input[i-1, j]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(bottom, top, left)
    }else if(i > 1 & i < 100 & j > 1 & j < 100){
      
      bottom <- mat_input[i,j] < mat_input[i + 1, j] 
      top <- mat_input[i,j] < mat_input[i-1, j]
      right <- mat_input[i,j] < mat_input[i, j + 1]
      left <- mat_input[i,j] < mat_input[i, j - 1]
      
      all_dirs <- c(bottom, top, right, left)
      
    }
    
    if(sum(all_dirs) == length(all_dirs)) {
      
      pos_map[i,j] <- "X"
      
    }
    
  }
  
}

write.csv(pos_map,here("nine/low_position_map.csv"))
sum(mat_input[pos_map] + 1)


# Part two
input_mat <- do.call(rbind, strsplit(readLines(here("nine/input.txt")), ""))
input_mat <- cbind("9", rbind("9", input_mat, "9"), "9")
n_rows <- nrow(input_mat)
input_vec <- as.numeric(input_mat)

lows_lgl <- 
  input_vec < lag(input_vec) &
  input_vec < lead(input_vec) &
  input_vec < lag(input_vec, n_rows) &
  input_vec < lead(input_vec, n_rows)

input_vec[lows_lgl] <- -seq(sum(lows_lgl))
nines <- input_vec == 9
while(any(input_vec %in% 1:8)) {
  
  input_vec <- pmin(
    input_vec,
    lag(input_vec),
    lead(input_vec),
    lag(input_vec, n_rows),
    lead(input_vec, n_rows)
  )
  
  input_vec[nines] <- 9
  
}

part2 <- prod(tail(sort(tabulate(-input_vec)), 3))

