
# Script 04: Extract Location Graphs from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)
library(igraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentDataFilename <- "COWebCommentData.rds"
coGroundTruthReviewedFilename <- "GroundTruth/COGroundTruthCommentsReviewed.json"

# create unique identifier for each location ----
coWebCommentData <- readRDS(file = file.path(dataPath, coWebCommentDataFilename)) |>
  dplyr::mutate(
    FullLocationName = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & AdditionalDescription != "NA" ~
        paste0(CardinalDirectionSubarea, " ", Name, " (", AdminLevel, "), (", AdditionalDescription, ")"),
      CardinalDirectionSubarea != "NA" & AdditionalDescription == "NA" ~
        paste0(CardinalDirectionSubarea, " ", Name, " (", AdminLevel, ")"),
      CardinalDirectionSubarea == "NA" & AdditionalDescription != "NA" ~
        paste0(Name, " (", AdminLevel, "), (", AdditionalDescription, ")"),
      .default = paste0(Name, " (", AdminLevel, ")")
    )
  ) |>
  dplyr::mutate(
    FullLocationName = gsub(
      pattern = "\\b(\\w+)(?:\\s+\\1\\b)+",
      replacement = "\\1",
      x = FullLocationName,
      perl = TRUE
    )
  )

# import ground truth comments for colorado ----
coGroundTruthReviewed <- jsonlite::read_json(path = file.path(dataPath, coGroundTruthReviewedFilename)) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned") |>
  dplyr::rename(AdditionalDescription = Description) |>
  dplyr::mutate(
    FullLocationName = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & AdditionalDescription != "NA" ~
        paste0(CardinalDirectionSubarea, " ", Name, " (", AdminLevel, "), (", AdditionalDescription, ")"),
      CardinalDirectionSubarea != "NA" & AdditionalDescription == "NA" ~
        paste0(CardinalDirectionSubarea, " ", Name, " (", AdminLevel, ")"),
      CardinalDirectionSubarea == "NA" & AdditionalDescription != "NA" ~
        paste0(Name, " (", AdminLevel, "), (", AdditionalDescription, ")"),
      .default = paste0(Name, " (", AdminLevel, ")")
    )
  )

# evaluate location recognition ----
evaluateLocationRecognition(
  groundTruthCommentData = coGroundTruthReviewed,
  comparisonCommentData = coWebCommentData
)

# evaluate location relationships ----
relationships <- evaluateLocationRelationships(
  comment = coWebCommentData |>
    dplyr::filter(CommentID == 4946) |>
    dplyr::pull(Comment) |>
    unique(),
  locationNames = coGroundTruthReviewed |>
    dplyr::filter(CommentID == 4946) |>
    dplyr::pull(FullLocationName)
)

locationNodes <- coGroundTruthReviewed |>
  dplyr::filter(CommentID == 4946) |>
  dplyr::pull(FullLocationName)

locationGraphs <- createLocationGraphs(
  locationNodes = locationNodes,
  relationships = relationships
)
