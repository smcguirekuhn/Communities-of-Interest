
# Script 01: Clean Georgia Web Comments Data

# reset global environment ----
rm(list = ls())

# import packages ----
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# assign import and export destinations ----
dataPath <- "./Data/Georgia/"
gaWebCommentsRawFilename <- "GA_Web_Comments_030922.txt"
gaWebCommentsProcessedFilename <- "GAWebComments.rds"

# clean and process georgia web comments ----
gaWebComments <- readr::read_lines(file = file.path(dataPath, gaWebCommentsRawFilename))[-1] |>
  dplyr::tibble(Line = _) |>
  dplyr::mutate(CommentID = cumsum(Line == "")) |>
  dplyr::filter(Line != "") |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(
    Header = dplyr::first(Line),
    Comment = ifelse(
      test = dplyr::n() > 1,
      yes = paste(Line[-1], collapse = " "),
      no = NA
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Header = Header |>
      stringr::str_trim() |>
      stringr::str_replace(pattern = '^"(.*)"$', replacement = '\\1'),
    Comment = Comment |>
      stringr::str_trim() |>
      stringr::str_replace(pattern = '^"(.*)"$', replacement = '\\1')
  ) |>
  dplyr::mutate(
    Date = Header |> stringr::str_extract(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}") |> lubridate::mdy(),
    Header = Header |> stringr::str_remove(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}"),
    Name = Header |> stringr::str_extract(pattern = "^.+(?=\\s+of\\s+)") |> stringr::str_to_lower(),
    County = Header |> stringr::str_extract(pattern = "(?<=\\sof\\s).+$"),
    .before = Comment
  ) |>
  dplyr::select(-Header) |>
  tidyr::drop_na(Comment) |>
  dplyr::mutate(Characters = nchar(Comment), .before = "Comment")

# save processed georgia web comments ----
saveRDS(object = gaWebComments, file = file.path(dataPath, gaWebCommentsProcessedFilename))
