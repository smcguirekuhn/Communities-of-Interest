
# Script 05: Extract Location Graphs from Colorado Web Comments

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
coWebCommentRelationshipsFilename <- "COWebCommentRelationships.rds"
coGroundTruthFilename <- "GroundTruth/COGroundTruthCommentsReviewed.json"

# import colorado web comment data ----
coWebCommentData <- readRDS(file = file.path(dataPath, coWebCommentDataFilename))

# import colorado web comment relationships ----
coWebCommentRelationships <- readRDS(file = file.path(dataPath, coWebCommentRelationshipsFilename))

# import colorado ground truth comments ----
coGroundTruthCommentData <- jsonlite::read_json(path = file.path(dataPath, coGroundTruthFilename))

# location recognition accuracy ----

## isolate colorado ground truth location mentions ----
coGroundTruthLocations <- coGroundTruthCommentData |>
  purrr::map(.f = \(webComment) webComment |> purrr::discard_at(at = "Relationships")) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# evaluate location recognition ----
locationRecognition <- evaluateLocationRecognition(
  groundTruthCommentData = coGroundTruthLocations,
  comparisonCommentData = coWebCommentData
)

# location relationship graph accuracy ----

## isolate colorado ground truth comment relationships ----
coGroundTruthRelationships <- coGroundTruthCommentData |>
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
locationGraphSimilarity <- purrr::map(
  .x = unique(coWebCommentData[["CommentID"]]),
  .f = \(commentID) {
    
    ### assign location nodes ----
    locationNodes <- coGroundTruthLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    
    ### calculate comment similarity ----
    if (length(locationNodes) > 1) {
      commentSimilarity <- evaluateLocationGraphSimilarity(
        locationNodes = locationNodes,
        groundTruthRelationships = coGroundTruthRelationships |>
          dplyr::filter(CommentID == commentID),
        comparisonRelationships = coWebCommentRelationships |>
          dplyr::filter(CommentID == commentID)
      )
    } else {
      commentSimilarity <- NULL
    }
    
    ### return comment similarity ----
    return(commentSimilarity)
  }
) |>
  purrr::set_names(nm = unique(coWebCommentData[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID")
