
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
    
    ## isolate location nodes and requests ----
    locationNodes <- allGroundTruthLocations |>
      dplyr::filter(State == state, CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    requests <- comparisonRequestsRaw |>
      dplyr::filter(State == state, CommentID == commentID) |>
      dplyr::select(Requests) |>
      tidyr::unnest(cols = Requests)
    
    ## create location graphs ----
    locationGraphs <- createLocationGraphs(
      locationNodes = locationNodes,
      requests = requests
    )
    
    ## format location requests ----
    locationRequests <- locationGraphs |>
      purrr::pluck("GroupedGraph") |>
      igraph::components() |>
      purrr::pluck("membership") |>
      tibble::enframe(name = "Location", value = "Membership") |>
      dplyr::mutate(
        Separations = purrr::map(
          .x = Location,
          .f = \(location) {
            locationGraphs |>
              purrr::pluck("SeparatedGraph") |>
              igraph::neighbors(v = location) |>
              names() |>
              tibble::as_tibble_col(column_name = "Locations")
          }
        )
      ) |>
      tidyr::nest(Groupings = Location) |>
      dplyr::select(-Membership) |>
      dplyr::relocate(Separations, .after = Groupings) |>
      dplyr::mutate(State = state, CommentID = commentID, .before = 1)
    
    ## return location requests ----
    return(locationRequests)
  }
)

evaluateABRequestAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  groundTruthRequests = allGroundTruthRequests,
  comparisonRequestsA = comparisonRequestsA,
  comparisonRequestsB = comparisonRequestsA
)
