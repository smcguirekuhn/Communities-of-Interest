
# Validation Tests for Ellmer Pipeline Scripts Across All States

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
ellmerOutputDataPath <- "./Validation/EllmerOutput/"
tablePath <- "./Tables/Validation/"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "AllGroundTruthRequests.rds"
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"
gaWebCommentLocationsFilename <- "GAWebCommentLocations.rds"
paWebCommentLocationsFilename <- "PAWebCommentLocations.rds"

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# import web comment locations data for all states ----
coWebCommentLocations <- readRDS(file = file.path(ellmerOutputDataPath, coWebCommentLocationsFilename))
gaWebCommentLocations <- readRDS(file = file.path(ellmerOutputDataPath, gaWebCommentLocationsFilename))
paWebCommentLocations <- readRDS(file = file.path(ellmerOutputDataPath, paWebCommentLocationsFilename))

# combine all web comment locations data ----
allWebCommentLocations <- dplyr::bind_rows(
  coWebCommentLocations |> dplyr::mutate(State = "Colorado", .before = 1),
  gaWebCommentLocations |> dplyr::mutate(State = "Georgia", .before = 1),
  paWebCommentLocations |> dplyr::mutate(State = "Pennsylvania", .before = 1)
)

# create first comparison set ----
comparisonLocationsA <- allWebCommentLocations |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(-c(Relevance, Iteration, Frequency, ContextualFrequency)) |>
  dplyr::ungroup()

# create second comparison set ----
comparisonLocationsB <- allWebCommentLocations |>
  dplyr::filter(ContextualFrequency <= 4, Iteration == 1) |>
  dplyr::select(-c(Relevance, Iteration, Frequency, ContextualFrequency)) |>
  dplyr::distinct(CommentID, FullLocationName, .keep_all = TRUE) |>
  dplyr::ungroup()

# evaluate recall contrast between comparison sets ----
evaluateABTestRecall(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB
)

# evaluate precision difference between comparison sets ----
evaluateABTestPrecision(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB
)
