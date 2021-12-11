#Advent of Code Day 10

library(tidyverse)
library(here)

#Part one

lines <- read_lines(here("ten/input.txt"))

old <- ''
while (!identical(old, lines -> old))
  lines <- gsub(r'(\(\)|<>|\{\}|\[\])', '', lines)
illegals <- regmatches(lines, regexpr(r'(\)|>|\}|\])', lines))

illegal_score <- c(')' = 3, ']' = 57, '}' = 1197, '>' = 25137)
sum(illegal_score[illegals])

#Part two
#Doesn't actually matter what the closing bracket is you just need to score each of the open brackets.
complete_score <- c('(' = 1, '[' = 2, '{' = 3, '<' = 4)

autocomplete <- function(lines) {
  old <- ''
  while (!identical(old, lines -> old))
    lines <- gsub(r'{\(\)|<>|\{\}|\[\]}', '', lines)
  illegals <- grep(r'{\)|>|\}|\]}', lines)
  chars <- strsplit(lines[-illegals], '')
  scores <- sapply(chars, Reduce, init = 0, right = TRUE,
                   f = function(c, s) 5 * s + complete_score[c])
  median(scores)
}

autocomplete(lines)
