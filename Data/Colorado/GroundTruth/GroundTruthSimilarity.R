
# Colorado Ground Truth Similarity

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(jsonlite)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coGroundTruthReviewedFilename <- "GroundTruth/COGroundTruthCommentsReviewed.json"
coGroundTruthComparisonFilename <- "GroundTruth/COGroundTruthComparison.json"

# import ground truth comments for colorado ----
coGroundTruthReviewed <- jsonlite::read_json(path = file.path(dataPath, coGroundTruthReviewedFilename)) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# import comparison comments for colorado ----
coGroundTruthComparison <- jsonlite::read_json(path = file.path(dataPath, coGroundTruthComparisonFilename)) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# calculate ground truth accuracy metrics ----
evaluateGroundTruthAccuracy(
  groundTruthComments = coGroundTruthReviewed,
  comparisonComments = coGroundTruthComparison
)

