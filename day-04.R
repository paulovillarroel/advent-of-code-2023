library(tidyverse)

input <- read_lines("data/day-04-input.txt")

input <- "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"


# Part 1
input_cleaned <- input |> 
  str_replace_all("( )+", " ") |> 
  str_replace_all("(\\b\\d\\b)", "0\\1") |> 
  str_split("\n") |> 
  unlist() |> 
  str_split("\\|", simplify = TRUE) |> 
  as.data.frame() |> 
  mutate_all(str_trim) |> 
  rename(winning = V1, numbers = V2) |> 
  filter(winning != "") |> 
  mutate(winning = str_remove(winning, "Card \\d+: ")) |> 
  mutate(winning = map(winning, ~ as.numeric(strsplit(.x, " ")[[1]])),
         numbers = map(numbers, ~ as.numeric(strsplit(.x, " ")[[1]])),
         selected_numbers = map2(numbers, winning, ~ intersect(.x, .y)),
         score = map_dbl(selected_numbers, ~ 2^(length(.) - 1)) |> floor())

sum(input_cleaned$score)


# Part 2


