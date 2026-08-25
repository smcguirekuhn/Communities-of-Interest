
# Script 05: Evaluate Ground Truth Accuracy of Georgia Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(jsonlite)
library(igraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia/"
gaWebCommentDataFilename <- "GAWebCommentData.rds"
gaWebCommentRelationshipsFilename <- "GAWebCommentRelationships.rds"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"

# import georgia web comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# import georgia web comment relationships ----
gaWebCommentRelationships <- readRDS(file = file.path(dataPath, gaWebCommentRelationshipsFilename))

# import georgia ground truth comments ----
gaGroundTruthCommentData <- jsonlite::read_json(
  path = file.path(dataPath, gaGroundTruthFilename),
  simplifyVector = TRUE
)

# location recognition accuracy ----

## isolate georgia ground truth location mentions ----
gaGroundTruthLocations <- gaGroundTruthCommentData |>
  dplyr::select(-Relationships) |>
  tidyr::unnest(col = "LocationsMentioned") |>
  dplyr::mutate_all(.funs = unlist)

# evaluate location recognition ----
locationRecognition <- evaluateLocationRecognition(
  groundTruthCommentData = gaGroundTruthLocations,
  comparisonCommentData = gaWebCommentData
)

# location relationship graph accuracy ----

## isolate georgia ground truth comment relationships ----
gaGroundTruthRelationships <- gaGroundTruthCommentData |>
  dplyr::select(CommentID, Relationships) |>
  dplyr::rowwise() |>
  dplyr::filter(length(Relationships) != 0) |>
  tidyr::unnest(col = "Relationships") |>
  dplyr::mutate_all(.funs = unlist)

## evaluate location graph similarity ----
locationGraphSimilarity <- evaluateGraphSimilarity(
  commentIDs = unique(gaWebCommentData[["CommentID"]]),
  groundTruthLocations = gaGroundTruthLocations,
  groundTruthRelationships = gaGroundTruthRelationships,
  comparisonRelationships = gaWebCommentRelationships
)
