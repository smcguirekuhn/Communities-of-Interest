
# Script 01: Clean Michigan Web Comments Data

# reset global environment ----
rm(list = ls())

# import packages ----
library(dplyr)
library(lubridate)

# assign import and export destinations ----
dataPath <- "./Data/Michigan"
miWebCommentsRawFilename <- "MIWebCommentsTabula.csv"
miWebCommentsProcessedFilename <- "MIWebComments.rds"

# clean and process tabula table for michigan web comments ----
miWebComments <- read.csv(file = file.path(dataPath, miWebCommentsRawFilename)) |>
  dplyr::select(
    Name = Name,
    Date = `Date.Provided`,
    Comment = Comment
  ) |>
  dplyr::mutate(
    CommentID = cumsum(
      x = grepl(
        pattern = "^.+$",
        x = dplyr::lag(Name)
      )
    )
  ) |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(
    Name = Name[dplyr::n()],
    Date = Date[dplyr::n()],
    Comment = paste(Comment, collapse = " ")
  ) |>
  dplyr::ungroup() |>
  tidyr::drop_na() |>
  dplyr::filter(Name != "", Date != "", Comment != "") |>
  dplyr::mutate(Date = lubridate::ymd_hms(Date) |> lubridate::as_date()) |>
  dplyr::distinct(Name, Date, Comment, .keep_all = TRUE) |>
  dplyr::arrange(Date) |>
  dplyr::mutate(CommentID = dplyr::row_number())

# save processed michigan comments ----
saveRDS(object = miWebComments, file = file.path(dataPath, miWebCommentsProcessedFilename))
