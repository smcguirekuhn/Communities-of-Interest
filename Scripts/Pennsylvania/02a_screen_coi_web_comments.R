
# Script 02a: Screen Pennsylvania Web Comments for Communities of Interest Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Pennsylvania/"
paWebCommentsFilename <- "PAWebComments.rds"

# import georgia web comments ----
paWebComments <- readRDS(file = file.path(dataPath, paWebCommentsFilename)) |>
  dplyr::filter(Comment != "") |>
  dplyr::distinct() |>
  dplyr::mutate(CommentID = dplyr::row_number(), .before = "Name")

# evaluate whether comments identify communities of interest ----
coiIndicators <- paWebComments[["Comment"]] |>
  as.list() |>
  screenCommentsForCOIs(state = "Pennsylvania")

# a community of interest screening column ----
paWebComments <- paWebComments |> dplyr::mutate(COI = coiIndicators)

# save web comments with coi indicators ----
saveRDS(object = paWebComments, file = file.path(dataPath, paWebCommentsFilename))
