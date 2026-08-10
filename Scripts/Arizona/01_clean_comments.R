
# Script 01: Clean Arizona Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Arizona/"
azWebCommentsRawFilename <- "AZ_All_Web_Comments.csv"
azWebCommentsProcessedFilename <- "AZWebComments.rds"

# import arizona web comments ----
azWebComments <- read.csv(file = file.path(dataPath, azWebCommentsRawFilename))

# clean and reformat arizona web comments ----
azWebComments <- azWebComments |>
  dplyr::slice(-c(1, 2)) |>
  dplyr::select(
    Name = `X.2`,
    Date = `X`,
    Comment = `X.4`
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(Date = lubridate::mdy_hm(Date) |> lubridate::as_date()) |>
  dplyr::filter(Comment != "") |>
  dplyr::distinct() |>
  dplyr::arrange(Date) |>
  dplyr::mutate(CommentID = dplyr::row_number(), .before = "Name")
  
# save processed arizona web comments ----
saveRDS(object = azWebComments, file = file.path(dataPath, azWebCommentsProcessedFilename))
