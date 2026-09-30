
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
requestedGroupsPath <- "./Validation/EllmerOutput/CommentRequests/RequestedGroups/"
pairwiseTableFilename <- "PairwiseValidationTable.rds"

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# import all ground truth requests data ----
allGroundTruthRequests <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthRequestsFilename))

# pairwise prompting vs requested groups testing ----

## compile requests from pairwise prompting ----
comparisonRequestsRaw <- list.files(path = file.path(pairwisePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  tidyr::nest(Requests = Location1:Confidence)

## format comparison requests from pairwise prompting ----
comparisonRequestsA <- purrr::map2_dfr(
  .x = comparisonRequestsRaw |> dplyr::pull(State),
  .y = comparisonRequestsRaw |> dplyr::pull(CommentID),
  .f = \(state, commentID) {
    mapPairwiseRequests(
      state = state,
      commentID = commentID,
      locationNodes = allGroundTruthLocations |>
        dplyr::filter(State == state, CommentID == commentID) |>
        dplyr::pull(FullLocationName),
      requests = comparisonRequestsRaw |>
        dplyr::filter(State == state, CommentID == commentID) |>
        dplyr::select(Requests) |>
        tidyr::unnest(cols = Requests)
    )
  }
)

## compile requests from requested groups prompting ----
comparisonRequestsRaw <- list.files(path = file.path(requestedGroupsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS)

## format comparison requests from requested groups prompting ----
comparisonRequestsB <- comparisonRequestsRaw |>
  dplyr::mutate(
    Groupings = purrr::map(
      .x = LocationsGrouped,
      .f = \(locationsGrouped) {
        if (length(locationsGrouped) == 0) {
          dplyr::tibble(Location = character(0))
        } else if (all(LocationsGrouped == "NA")) {
          dplyr::tibble(Location = character(0))
        } else {
          dplyr::tibble(Location = as.character(locationsGrouped)) |>
            dplyr::filter(Location != "NA")
        }
      }
    ),
    Separations = purrr::map(
      .x = LocationsSeparated,
      .f = \(locationsSeparated) {
        if (length(locationsSeparated) == 0) {
          dplyr::tibble(Location = character(0))
        } else if (all(locationsSeparated == "NA")) {
          dplyr::tibble(Location = character(0))
        } else {
          dplyr::tibble(Location = as.character(locationsSeparated)) |>
            dplyr::filter(Location != "NA")
        }
      }
    )
  ) |>
  dplyr::select(State, CommentID, Groupings, Separations)

## conduct pairwise prompting vs requested groups regression tests ----
evaluateABRequestAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  groundTruthRequests = allGroundTruthRequests,
  comparisonRequestsA = comparisonRequestsA,
  comparisonRequestsB = comparisonRequestsB
) |> saveRDS(file = file.path(tablePath, pairwiseTableFilename))
