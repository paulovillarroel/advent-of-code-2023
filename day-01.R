library(tidyverse)

data <- read_lines("data/day-01-input.txt")

# Part 1
data |>
  as_tibble() |>
  extract(value, "first", "(\\d)", remove = FALSE) |>
  extract(value, "last", ".*(\\d)") |>
  mutate(value = as.numeric(paste0(first, last))) |>
  summarise(sum(value))


# Part 2
word_to_numbers - function(word) {
  mapping <- c(
    "one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5",
    "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9"
  )

  for (i in seq_along(mapping)) {
    word <- str_replace_all(word, mapping[i], digits_letters[i])
  }

  return(word)
}

data |>
  as_tibble() |>
  extract(value, "first", "(\\d|one|two|three|four|five|six|seven|eight|nine)", remove = FALSE) |>
  extract(value, "last", ".*(\\d|one|two|three|four|five|six|seven|eight|nine)") |>
  mutate(
    first = word_to_numbers(first),
    last = word_to_numbers(last),
    value = as.numeric(paste0(first, last))
  ) |>
  summarise(sum(value))
