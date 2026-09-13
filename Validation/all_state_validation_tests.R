
# Validation Tests for Ellmer Pipeline Scripts Across All States

# reset global environment ----
rm(list = ls())

# import packages ----
library(joyn)
library(purrr)
library(tidyr)
library(dplyr)
library(jsonlite)
library(estimatr)
library(igraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
groundTruthDataPath <- "./Validation/GroundTruth/"
ellmerOutputDataPath <- "./Validation/EllmerOutput/"
tablePath <- "./Tables/Validation/"
coGroundTruthFilename <- "COGroundTruthCommentData.json"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"
gaWebCommentLocationsFilename <- "GAWebCommentLocations.rds"

# # import colorado ground truth data ----
# coGroundTruthData <- jsonlite::read_json(
#   path = file.path(groundTruthDataPath, coGroundTruthFilename),
#   simplifyVector = TRUE
# )
# 
# # format colorado ground truth locations ----
# coGroundTruthLocations <- coGroundTruthData |>
#   dplyr::select(CommentID, LocationsMentioned) |>
#   tidyr::unnest(cols = LocationsMentioned) |>
#   dplyr::mutate(
#     SubareaDescription = dplyr::case_when(
#       CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
#       AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
#       .default = "NA"
#     ),
#     .before = "FullLocationName"
#   ) |>
#   dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
#   dplyr::mutate(State = "Colorado", .before = "CommentID")
# 
# # import georgia ground truth data ----
# gaGroundTruthData <- jsonlite::read_json(
#   path = file.path(groundTruthDataPath, gaGroundTruthFilename),
#   simplifyVector = TRUE
# )
# 
# # format georgia ground truth locations ----
# gaGroundTruthLocations <- gaGroundTruthData |>
#   dplyr::select(CommentID, LocationsMentioned) |>
#   dplyr::mutate(CommentID = as.integer(CommentID)) |>
#   tidyr::unnest(cols = LocationsMentioned) |>
#   dplyr::mutate(
#     SubareaDescription = dplyr::case_when(
#       CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
#       AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
#       .default = "NA"
#     ),
#     .before = "FullLocationName"
#   ) |>
#   dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
#   dplyr::mutate(State = "Georgia", .before = "CommentID")
# 
# # combine all ground truth locations data ----
# allGroundTruthLocations <- dplyr::bind_rows(
#   coGroundTruthLocations,
#   gaGroundTruthLocations
# )
# 
# # save all ground truth locations data ----
# saveRDS(allGroundTruthLocations, file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# import all ground truth locations data ----
allGroundTruthLocations <- readRDS(file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# import colorado web comment locations data ----
coWebCommentLocations <- readRDS(file = file.path(ellmerOutputDataPath, coWebCommentLocationsFilename))

# import georgia web comment locations data ----
gaWebCommentLocations <- readRDS(file = file.path(ellmerOutputDataPath, gaWebCommentLocationsFilename))

# combine all web comment locations data ----
allWebCommentLocations <- dplyr::bind_rows(
  coWebCommentLocations |> dplyr::mutate(State = "Colorado", .before = 1),
  gaWebCommentLocations |> dplyr::mutate(State = "Georgia", .before = 1)
)

# create first comparison set ----
comparisonLocationsA <- allWebCommentLocations |>
  dplyr::filter(Iteration == 1) |>
  dplyr::select(-c(Relevance, Iteration, Frequency, ContextualFrequency)) |>
  dplyr::ungroup()

# create second comparison set ----
comparisonLocationsB <- allWebCommentLocations |>
  dplyr::filter(ContextualFrequency <= 3, Iteration == 1) |>
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
