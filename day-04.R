library(tidyverse)

input <- read_lines("data/day-04-input.txt")

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
  mutate(
    winning = map(winning, ~ as.numeric(strsplit(.x, " ")[[1]])),
    numbers = map(numbers, ~ as.numeric(strsplit(.x, " ")[[1]])),
    selected_numbers = map2(numbers, winning, ~ intersect(.x, .y)),
    score = map_dbl(selected_numbers, ~ 2^(length(.) - 1)) |> floor()
  )

sum(input_cleaned$score)


# Part 2
games <- input_cleaned |>
  mutate(
    matches = map_int(selected_numbers, ~ length(.)),
    id = row_number()
  ) |>
  select(id, matches)

count_cards <- function(processed_data) {
  matches <- select(processed_data, id, matches) |>
    mutate(cards_count = 1) |>
    as.data.frame()
  for (i in 1:nrow(matches)) {
    match_count <- matches$matches[[i]]
    cards_count <- matches$cards_count[[i]]
    if (match_count != 0) {
      matches$cards_count[(i + 1):(i + match_count)] <-
        matches$cards_count[(i + 1):(i + match_count)] + cards_count
    }
  }
  sum(matches$cards_count)
}

count_cards(games)
