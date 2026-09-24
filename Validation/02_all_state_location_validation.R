
# Location Validation Tests for Ellmer Pipeline Scripts Across All States

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
noSystemPromptPath <- "./Validation/EllmerOutput/CommentLocations/NoSystemPrompt/"
iterationsPath <- "./Validation/EllmerOutput/CommentLocations/Iterations/"
tablePath <- "./Tables/Validation/"
systemPromptTableFilename <- "SystemPromptValidationTable.rds"
iterationsTableFilename <- "IterationsValidationTable.rds"
contextualTableFilename <- "ContextualValidationTable.rds"

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# system prompt testing ----

## compile locations extracted with a system prompt ----
comparisonLocationsA <- list.files(path = file.path(iterationsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted without a system prompt ----
comparisonLocationsB <- list.files(path = file.path(noSystemPromptPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, systemPromptTableFilename))

# majority across iterations testing ----

## compile locations for a single iteration ----
comparisonLocationsA <- list.files(path = file.path(iterationsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations mentioned across a majority of multiple iterations ----
comparisonLocationsB <- list.files(path = file.path(iterationsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Frequency >= 2) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName) |>
  dplyr::distinct(CommentID, FullLocationName, .keep_all = TRUE)

## conduct majority across iterations regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, iterationsTableFilename))

# contextual location removal testing ----

## compile all locations for a single iteration ----
comparisonLocationsA <- list.files(path = file.path(iterationsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile all locations for a single iteration classified as relevant ----
comparisonLocationsB <- list.files(path = file.path(iterationsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct contextual location removal regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, contextualTableFilename))
