
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
baselinePath <- "./Validation/EllmerOutput/CommentLocations/Baseline/"
baselineNoRelevance <- "./Validation/EllmerOutput/CommentLocations/MoreExamplesNoContextual/"
simplerPath <- "./Validation/EllmerOutput/CommentLocations/Simpler/"
noSystemPromptPath <- "./Validation/EllmerOutput/CommentLocations/NoSystemPrompt/"
groupIDsPath <- "./Validation/EllmerOutput/CommentLocations/GroupIDs/"
tablePath <- "./Tables/Validation/LocationRecognition/"
contextualRemovalTableFilename <- "ContextualRemovalValidationTable.rds"
majorityMentioningTableFilename <- "MajorityMentioningValidationTable.rds"
unanimousMentioningTableFilename <- "UnanimousMentioningValidationTable.rds"
baselineSimplerTableFilename <- "BaselineSimplerValidationTable.rds"
systemPromptTableFilename <- "SystemPromptValidationTable.rds"
groupIDsTableFilename <- "GroupIDValidationTable.rds"

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))


# baseline testing for contextual locations removal ----

## compile locations extracted with baseline prompting ----
comparisonLocationsA <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted with baseline prompting and contextual location removal ----
comparisonLocationsB <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1, Relevance == "relevant") |> 
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, contextualRemovalTableFilename))


# baseline testing for majority mentions testing ----

## compile locations extracted with baseline prompting and contextual location removal ----
comparisonLocationsA <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted baseline prompting, contextual location removal, and majority mentions ----
comparisonLocationsB <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Frequency >= 2, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName) |>
  dplyr::distinct(CommentID, FullLocationName, .keep_all = TRUE)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, majorityMentioningTableFilename))


# baseline testing for unanimous mentions testing ----

## compile locations extracted with baseline prompting and contextual location removal ----
comparisonLocationsA <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted with baseline prompting, contextual location removal, and unanimous mentions ----
comparisonLocationsB <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Frequency == 3, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName) |>
  dplyr::distinct(CommentID, FullLocationName, .keep_all = TRUE)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, unanimousMentioningTableFilename))


# baseline testing against simpler prompting architecture ----

## compile locations extracted with baseline prompting ----
comparisonLocationsA <- list.files(path = file.path(baselinePath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 2, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted with simpler prompting architecture ----
comparisonLocationsB <- list.files(path = file.path(simplerPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 2, Relevance == "relevant") |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, baselineSimplerTableFilename))


# system prompt testing ----

## compile locations extracted without a system prompt ----
comparisonLocationsA <- list.files(path = file.path(noSystemPromptPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile locations extracted with a system prompt ----
comparisonLocationsB <- list.files(path = file.path(simplerPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct system prompt regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, systemPromptTableFilename))


# group ids inclusion testing ----

## compile locations without group id prompting ----
comparisonLocationsA <- list.files(path = file.path(simplerPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## compile all locations with group id prompting ----
comparisonLocationsB <- list.files(path = file.path(groupIDsPath), full.names = TRUE) |>
  purrr::map_dfr(.f = readRDS) |>
  dplyr::select(State, CommentID, AdminLevel, Name, SubareaDescription, FullLocationName)

## conduct contextual location removal regression tests ----
evaluateABLocationAccuracy(
  groundTruthLocations = allGroundTruthLocations,
  comparisonLocationsA = comparisonLocationsA,
  comparisonLocationsB = comparisonLocationsB,
  unit = "location"
) |> saveRDS(file = file.path(tablePath, groupIDsTableFilename))
