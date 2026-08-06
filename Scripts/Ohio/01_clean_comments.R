
# Script 01: Clean Ohio Web Comments Data

# reset global environment ----
rm(list = ls())

# import packages ----
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# assign import and export destinations ----
dataPath <- "./Data/Ohio/"
ohWebCommentsRawFilename <- "OH_Web_Comments_032121.csv"
ohWebCommentsProcessedFilename <- "OHWebComments.rds"

# clean and process georgia web comments ----
ohWebComments <- readr::read_lines(file = file.path(dataPath, ohWebCommentsRawFilename)) |>
  dplyr::tibble(Line = _) |>
  dplyr::mutate(
    CommentID = cumsum(
      x = grepl(
        pattern = "^\"[A-Z][a-z]{2} \\d{1,2}, \\d{4}",
        x = dplyr::lead(Line)
      )
    )
  ) |>
  dplyr::filter(Line != "") |>
  dplyr::mutate(
    Line = Line |>
      stringr::str_trim() |>
      stringr::str_replace(pattern = '^"(.*)"$', replacement = '\\1')
  ) |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(
    Name = dplyr::first(Line),
    Date = Line[2],
    Comment = ifelse(
      test = dplyr::n() > 1,
      yes = paste(Line[-c(1:2)], collapse = " "),
      no = NA
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(Date = Date |> stringr::str_replace_all("\\s+", " ") |> lubridate::mdy_hm()) |>
  tidyr::drop_na(Comment)

# save processed georgia web comments ----
saveRDS(object = ohWebComments, file = file.path(dataPath, ohWebCommentsProcessedFilename))
