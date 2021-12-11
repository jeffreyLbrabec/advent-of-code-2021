#Advent of Code Day 10

library(tidyverse)
library(here)

#Part one

input <- c("[({(<(())[]>[[{[]{<()<>>",
           "[(()[<>])]({[<{<<[]>>(",
           "{([(<{}[<>[]}>{[]{[(<()>",
           "(((({<>}<{<{<>}{[]{[]{}",
           "[[<[([]))<([[{}[[()]]]",
           "[{[{({}]{}}([{[{{{}}([]",
           "{<[[]]>}<{[{[{[]{()[[[]",
           "[<(<(<(<{}))><([]([]()",
           "<{([([[(<>()){}]>(<<{{",
           "<{([{{}}[<[[[<>{}]]]>[]]") %>% 
  as_tibble()

str_detect(input$value, "\\(\\)|\\[\\]|\\{\\}|<>")
str_detect(input$value, "\\(|\\[|\\{|<")

huh <- input %>% 
  mutate(value = str_split(value, pattern = ""),
         value = map())
  filter()
