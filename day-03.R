library(tidyverse)

input <- readLines("data/day-03-input.txt")


# Part 1
mat <- sapply(input, \(x) strsplit(x, ""), USE.NAMES = FALSE) |>
  stringi::stri_list2matrix(byrow = TRUE)

extract_number <- function(row, row_index, mat) {
  number_indices <- grep("[0-9]", row)
  if (length(number_indices) == 0) {
    return(NULL)
  }

  groups <- cumsum(c(0, diff(number_indices)) != 1)
  res <- tapply(number_indices, groups, function(x) {
    adjacent_values <- mat[
      max(row_index - 1, 1):min(row_index + 1, nrow(mat)),
      max(min(x) - 1, 1):min(max(x) + 1, ncol(mat))
    ]
    if (any(!grepl("[0-9.]", adjacent_values))) {
      paste0(mat[row_index, min(x):max(x)], collapse = "") |> as.integer()
    }
  })
  return(unlist(res))
}

valid_numbers <- sapply(seq_len(nrow(mat)), function(i) extract_number(mat[i, ], i, mat))
result <- sum(unlist(valid_numbers), na.rm = TRUE)

result

# Part 2
extract_gears <- function(mat) {
  all_numbers <- list()
  for (i in 1:nrow(mat)) {
    star_indices <- grep("\\*", mat[i, ])

    if (length(star_indices) > 0) {
      gear_number <- lapply(
        star_indices,
        function(star) {
          adjacent_values <-
            mat[
              max(i - 1, 1):min(i + 1, nrow(mat)),
              max(star - 1, 1):min(star + 1, ncol(mat))
            ]

          numbers <- find_numbers(mat, i, star)
          if (length(numbers) == 2) {
            prod(numbers)
          }
        }
      )
      all_numbers <- append(all_numbers, gear_number)
    }
  }
  return(all_numbers)
}

find_numbers <- function(mat, row, col) {
  rows <- max(row - 1, 1):min(row + 1, nrow(mat))
  cols <- max(col - 1, 1):min(col + 1, ncol(mat))

  adjacent_number_idx <-
    apply(
      mat[rows, cols],
      1,
      function(line) {
        idx <- grep("[0-9]", line)
        if (length(idx) > 0) {
          idx - 2 + col
        }
      }
    )
  numbers <- list()
  for (i in seq_along(adjacent_number_idx)) {
    has_number <- !is.null(adjacent_number_idx[[i]])
    if (has_number) {
      idx <- grep("[0-9]", mat[rows[i], ])
      grps <- cumsum(c(0, diff(idx)) != 1)
      res <- tapply(idx, grps, function(x) {
        is_valid <- any(adjacent_number_idx[[i]] %in% x)
        if (is_valid) {
          paste0(mat[rows[i], min(x):max(x)], collapse = "")
        }
      }) |> unname()
      numbers <- append(numbers, res)
      res <- NULL
    }
  }
  out <- unlist(numbers) |> as.integer()
  return(out)
}

extract_gears(mat) |>
  unlist() |>
  sum()
