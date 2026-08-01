
# Ground Truth Coding for Georgia Comments

# progress so far: 35 comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)

# assign import and export destinations ----
dataPath <- "./Data/Georgia/"
groundTruthOriginalFileName <- "GroundTruth/GAGroundTruthComments.rds"
groundTruthEditedFileName <- "GroundTruth/GAGroundTruthComments.json"

# write ground truth comments to json file and make edits as needed ----
readRDS(file = file.path(dataPath, groundTruthOriginalFileName)) |>
  jsonlite::write_json(path = file.path(dataPath, groundTruthEditedFileName), pretty = TRUE)
