
# Request Validation Tests for Ellmer Pipeline Scripts Across All States

# reset global environment ----
rm(list = ls())

# import packages ----
library(joyn)
library(purrr)
library(tidyr)
library(tibble)
library(dplyr)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
groundTruthDataPath <- "./Validation/GroundTruth/"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "AllGroundTruthRequests.rds"
tablePath <- "./Tables/Validation/"
pairwisePath <- "./Validation/EllmerOutput/CommentRequests/Pairwise/"

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# import all ground truth requests data ----
allGroundTruthRequests <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthRequestsFilename))

## compile requests from pairwise prompting ----
comparisonRequestsRaw <- list.files(path = file.path(pairwisePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  tidyr::nest(Requests = Location1:Confidence)

# format all comparison requests ----
comparisonRequestsA <- purrr::map2_dfr(
  .x = comparisonRequestsRaw |> dplyr::pull(State),
  .y = comparisonRequestsRaw |> dplyr::pull(CommentID),
  .f = \(state, commentID) {
    mapPairwiseRequests(
      state = state,
      commentID = commentID,
      locationNodes = coGroundTruthLocations |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::pull(FullLocationName),
      requests = coGroundTruthData |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::select(Requests) |>
        tidyr::unnest(cols = Requests)
    )
  }
)

evaluateABRequestAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  groundTruthRequests = allGroundTruthRequests,
  comparisonRequestsA = comparisonRequestsA,
  comparisonRequestsB = comparisonRequestsA
)
