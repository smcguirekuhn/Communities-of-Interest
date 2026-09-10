
# Script 02: Screen Colorado Web Comments for Communities of Interest Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentsFilename <- "COWebComments.rds"

# import georgia web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# evaluate whether comments identify communities of interest ----
coiIndicators <- coWebComments[["Comment"]] |>
  as.list() |>
  screenCommentsForCOIs(state = "Colorado")

# a community of interest screening column ----
coWebComments <- coWebComments |> dplyr::mutate(COI = coiIndicators)

# save web comments with coi indicators ----
saveRDS(object = coWebComments, file = file.path(dataPath, coWebCommentsFilename))
