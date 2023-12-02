library(tidyverse)

data <- read_lines("data/day-02-input.txt")

data_2 <- data |>
  as_tibble() |>
  mutate(
    game = row_number(),
    cubes = str_extract_all(value, "\\d+ [a-z]+")
  ) |>
  unnest(cubes) |>
  separate(cubes, into = c("count", "color"), sep = " ", convert = TRUE) |>
  select(-value)


# Part 1
bag <- data.frame(
  count = c(12, 13, 14),
  color = c("red", "green", "blue")
)

data_2 |>
  left_join(bag, by = "color") |>
  mutate(is_possible = count.x <= count.y) |>
  group_by(game) |>
  summarise(is_possible = all(is_possible)) |>
  filter(is_possible == TRUE) |>
  summarise(sum_id = sum(game))


# Part 2
data_2 |>
  group_by(game, color) |>
  summarise(max_cubes = max(count)) |>
  summarise(power = prod(max_cubes)) |>
  summarise(sum_power = sum(power))
