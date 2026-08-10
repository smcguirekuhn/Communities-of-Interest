
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
coScrapedWebCommentsFilename <- "COScrapedWebComments.rds"
coWebCommentsProcessedFilename <- "COWebComments.rds"

# import cora request web comments ----
coCORAWebComments <- read.csv(file = file.path(dataPath, coWebCommentsRawFilename))

# clean and reformat cora request web comments ----
coCORAWebComments <- coCORAWebComments |>
  dplyr::select(
    Name = name,
    Date = updated_at,
    Commission = commission,
    ZIPCode = zip,
    Comment = comment
  ) |>
  tidyr::drop_na(ZIPCode, Comment, Date) |>
  dplyr::mutate(Date = lubridate::ymd_hms(Date) |> lubridate::as_date()) |>
  dplyr::filter(Comment != "") |>
  dplyr::distinct(Name, ZIPCode, Comment, .keep_all = TRUE)

# import scraped comments ----
coScrapedWebComments <- readRDS(file = file.path(dataPath, coScrapedWebCommentsFilename))

# combine cora web comments and scraped web comments ----
coWebComments <- dplyr::bind_rows(coCORAWebComments, coScrapedWebComments) |>
  dplyr::distinct(Name, Commission, ZIPCode, Date, .keep_all = TRUE) |>
  dplyr::select(-PageID) |>
  tidyr::drop_na() |>
  dplyr::arrange(Date) |>
  dplyr::mutate(CommentID = dplyr::row_number(), .before = "Name")
  
# save processed colorado web comments ----
saveRDS(object = coWebComments, file = file.path(dataPath, coWebCommentsProcessedFilename))
