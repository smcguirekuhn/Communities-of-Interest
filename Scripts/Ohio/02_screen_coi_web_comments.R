
# Script 02: Screen Ohio Web Comments for Communities of Interest Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Ohio/"
ohWebCommentsFilename <- "OHWebComments.rds"

# import ohio web comments ----
ohWebComments <- readRDS(file = file.path(dataPath, ohWebCommentsFilename)) |>
  dplyr::filter(Comment != "") |>
  dplyr::distinct()

# evaluate whether comments identify communities of interest ----
coiIndicators <- ohWebComments[["Comment"]] |>
  as.list() |>
  screenCommentsForCOIs(state = "Ohio")

# a community of interest screening column ----
ohWebComments <- ohWebComments |> dplyr::mutate(COI = coiIndicators)

# save web comments with coi indicators ----
saveRDS(object = ohWebComments, file = file.path(dataPath, ohWebCommentsFilename))
