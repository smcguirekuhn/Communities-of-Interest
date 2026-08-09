
# Script 01b: Clean Colorado Web Comments from CORA Request

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
dataPath <- "./Data/Colorado/"
coWebCommentsRawFilename <- "CORACommentsColorado.csv"
coWebCommentsProcessedFilename <- "COWebComments.rds"

# import colorado web comments ----
coWebComments <- read.csv(file = file.path(dataPath, coWebCommentsRawFilename))

# clean and reformat colorado web comments ----
coWebComments <- coWebComments |>
  dplyr::select(
    Name = name,
    Date = updated_at,
    Commission = commission,
    ZIPCode = zip,
    Comment = comment
  ) |>
  tidyr::drop_na(ZIPCode, Comment, Date) |>
  dplyr::mutate(Date = lubridate::ymd_hms(Date)) |>
  dplyr::filter(Comment != "") |>
  dplyr::distinct(Name, ZIPCode, Comment, .keep_all = TRUE)

# save processed colorado web comments ----
saveRDS(object = coWebComments, file = file.path(dataPath, coWebCommentsProcessedFilename))
