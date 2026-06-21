
# Script 01a: Clean Pennsylvania House Comments Data

# reset global environment ----
rm(list = ls())

# import packages ----
library(dplyr)
library(lubridate)

# assign import and export destinations ----
dataPath <- "./Data/Pennsylvania"
paHouseCommentsRawFilename <- "PAHouseCommentsTabula.csv"
paHouseCommentsProcessedFilename <- "PAHouseComments.rds"

# clean and process tabula table for pennsylvania house redistricting comments ----
paHouseComments <- read.csv(file = file.path(dataPath, paHouseCommentsRawFilename)) |>
  dplyr::select(-X) |>
  dplyr::rename(CommunityName = "Community.Name") |>
  dplyr::mutate(
    Description = gsub(
      pattern = "\n",
      replacement = " ",
      x = paste0(CommunityName, ": ", Description)
    ),
    Date = lubridate::dmy(x = Date)
  )

# save processed pennsylvania comments ----
saveRDS(object = paHouseComments, file = file.path(dataPath, paHouseCommentsProcessedFilename))
