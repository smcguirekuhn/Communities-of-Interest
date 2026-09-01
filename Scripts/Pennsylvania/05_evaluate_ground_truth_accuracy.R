
# Script 05: Evaluate Ground Truth Accuracy of Pennsylvania Web Comments

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
dataPath <- "./Data/Pennsylvania/"
paWebCommentDataFilename <- "PAWebCommentData.rds"
paWebCommentRelationshipsFilename <- "PAWebCommentRelationships.rds"
paGroundTruthFilename <- "PAGroundTruthCommentData.json"

# import pennsylvania web comment data ----
paWebCommentData <- readRDS(file = file.path(dataPath, paWebCommentDataFilename))

# import pennsylvania web comment relationships ----
paWebCommentRelationships <- readRDS(file = file.path(dataPath, paWebCommentRelationshipsFilename))

# import pennsylvania ground truth comments ----
paGroundTruthCommentData <- jsonlite::read_json(path = file.path(dataPath, paGroundTruthFilename))

# location recognition accuracy ----

## isolate pennsylvania ground truth location mentions ----
paGroundTruthLocations <- paGroundTruthCommentData |>
  purrr::map(.f = \(webComment) webComment |> purrr::discard_at(at = "Relationships")) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# evaluate location recognition ----
locationRecognition <- evaluateLocationRecognition(
  groundTruthCommentData = paGroundTruthLocations,
  comparisonCommentData = paWebCommentData
)

# location relationship graph accuracy ----

## isolate pennsylvania ground truth comment relationships ----
paGroundTruthRelationships <- paGroundTruthCommentData |>
  purrr::map(
    .f = \(webComment) {
      webCommentRelationships <- webComment[["Relationships"]] |> dplyr::bind_rows()
      if (nrow(webCommentRelationships) > 0) {
        webCommentRelationships <- webCommentRelationships |>
          dplyr::mutate(CommentID = webComment[["CommentID"]], .before = "Location1")
      }
      return(webCommentRelationships)
    }
  ) |>
  purrr::list_rbind()

## evaluate location graph similarity ----
locationGraphSimilarity <- evaluateGraphSimilarity(
  commentIDs = unique(paWebCommentData[["CommentID"]]),
  groundTruthLocations = paGroundTruthLocations,
  groundTruthRelationships = paGroundTruthRelationships,
  comparisonRelationships = paWebCommentRelationships
)
