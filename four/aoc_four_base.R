#Advent of Code Day Four

library(tidyverse)
library(here)

#Part One
bingo_nums <- scan(here("four/input.txt"), sep = ',', nlines = 1, quiet = TRUE)
cards <- read.table(here("four/input.txt"), skip = 1)



score_card <- function(mat, draw) {
  
  marked <- is.na(mat)
  if(all(c(rowMeans(marked), colMeans(marked)) != 1))
    return(0)
  sum(mat, na.rm = TRUE) * draw
}

play_bingo <- function(draws, cards) {
  
  size <- ncol(cards)
  ncards <- nrow(cards)/size
  ids <- rep(1:ncards, each = size)
  for(d in draws) {
    
    cards[cards == d] <- NA
    score <- sapply(split(cards, ids), score_card, draw = d)
    if(any(score>0))
      return(score[score>0])
  }
}

play_bingo(draws = bingo_nums, cards = cards)

## Part 2

play_bingo2 <- function(draws, cards) {
  
  size <- ncol(cards)
  for(d in draws) {
    
    ncards <- nrow(cards)/size
    ids <- rep(1:ncards, each = size)
    cards[cards == d] <- NA
    score <- sapply(split(cards, ids), score_card, draw = d)
    if(any(score > 0)) {
      if(ncards == 1)
        return(score[score > 0])
      cards <- cards[ids %in% which(score == 0),]
    }
    
  }
  
}

play_bingo2(draws = bingo_nums, cards)
