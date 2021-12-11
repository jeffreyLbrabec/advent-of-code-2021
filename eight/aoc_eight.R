#Advent of Code Day 8

library(tidyverse)
library(here)

# Part one
input <- read_lines(here("eight/input.txt"))

input %>% 
  str_split("\\|") %>%
  map(str_trim) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  rename(input = V1, output = V2) %>% 
  select(output) %>%
  separate(output, into = as.character(c(1:4)), sep = " ") %>% 
  pivot_longer(cols = everything(), names_to = "names", values_to = "outputs") %>% 
  mutate(num_seg = str_count(outputs)) %>% 
  filter(num_seg == 2 | num_seg == 3 | num_seg == 4 | num_seg == 7) %>% 
  summarize(n_uniq_digits = n())

# Part two

input_split <- input %>% 
  str_split("\\|") %>%
  map(str_trim) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  rename(input = V1, output = V2)
  
input_segmented <- input_split %>%   
  select(input) %>% 
  separate(input, into = as.character(c(1:10)), sep = " ") %>% 
  t() %>% 
  as_tibble() %>% 
  pivot_longer(everything(), names_to = "names", values_to = "segs") %>% 
  mutate(num_seg = str_count(segs)) %>%
  nest(pat = segs:num_seg)

outputs <- input_split %>% 
  select(output) %>% 
  separate(output, into = as.character(c(1:4)), sep = " ") %>% 
  t() %>% 
  as_tibble() %>% 
  pivot_longer(everything(), names_to = "names", values_to = "segs") %>% 
  nest(digis = segs)
 
input_keyed <- input_segmented %>% 
  mutate(pat = map(pat, decipher_num))

outputs %>% 
  full_join(input_keyed, by = "names") %>% 
  mutate(digis = map2(digis, pat, get_num)) %>% 
  mutate(num = map(digis, select, num_key)) %>% 
  select(names, num) %>% 
  unnest(num) %>% 
  # pivot_wider(names_from = names, values_from = num_key) %>% 
  # map(unlist)
  group_by(names) %>% 
  summarize(full_num = parse_number(str_c(num_key, collapse = ""))) %>% 
  summarize(sum = sum(full_num))

decipher_num <- function(data) {
  
  data %>% 
    rowwise() %>% 
    mutate(num_key = case_when(num_seg == 6 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 4) %>% select(segs), "", simplify = TRUE)) == 3 &
                                 sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 2) %>% select(segs), "", simplify = TRUE)) == 2 ~ 0,
                               num_seg == 2 ~ 1,
                               num_seg == 5 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 4) %>% select(segs), "", simplify = TRUE)) == 2 ~ 2,
                               num_seg == 5 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 4) %>% select(segs), "", simplify = TRUE)) == 3 & 
                                 sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 2) %>% select(segs), "", simplify = TRUE)) == 2 ~ 3,
                               num_seg == 4 ~ 4,
                               num_seg == 5 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 4) %>% select(segs), "", simplify = TRUE)) == 3 & 
                                 sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 2) %>% select(segs), "", simplify = TRUE)) == 1 ~ 5,
                               num_seg == 6 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 2) %>% select(segs), "", simplify = TRUE)) == 1 ~ 6,
                               num_seg == 3 ~ 7,
                               num_seg == 7 ~ 8,
                               num_seg == 6 & sum(str_split(segs, "", simplify = TRUE) %in% str_split(filter(., str_count(segs) == 4) %>% select(segs), "", simplify = TRUE)) == 4 ~ 9)) %>% 
    ungroup()
  
}

get_num <- function(digi_tib, pat_tib) {
  
  digi_ord <- digi_tib %>% 
    mutate(segs = str_split(segs, ""), segs = map(segs, str_sort))
  
  pat_ord <- pat_tib %>% 
    mutate(segs = str_split(segs, ""), segs = map(segs, str_sort))
    
  inner_join(digi_ord, pat_ord, by = "segs")
  
}
