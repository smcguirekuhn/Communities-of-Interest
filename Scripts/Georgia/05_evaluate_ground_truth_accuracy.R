
# Script 05: Evaluate Ground Truth Accuracy of Georgia Web Comments

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
dataPath <- "./Data/Georgia/"
gaWebCommentDataFilename <- "GAWebCommentData.rds"
gaWebCommentRelationshipsFilename <- "GAWebCommentRelationships.rds"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"

# import georgia web comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# import georgia web comment relationships ----
gaWebCommentRelationships <- readRDS(file = file.path(dataPath, gaWebCommentRelationshipsFilename))

# import georgia ground truth comments ----
gaGroundTruthCommentData <- jsonlite::read_json(path = file.path(dataPath, gaGroundTruthFilename))

# location recognition accuracy ----

## isolate georgia ground truth location mentions ----
gaGroundTruthLocations <- gaGroundTruthCommentData |>
  purrr::map(.f = \(webComment) webComment |> purrr::discard_at(at = "Relationships")) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# evaluate location recognition ----
locationRecognition <- evaluateLocationRecognition(
  groundTruthCommentData = gaGroundTruthLocations,
  comparisonCommentData = gaWebCommentData
)

# location relationship graph accuracy ----

## isolate georgia ground truth comment relationships ----
gaGroundTruthRelationships <- gaGroundTruthCommentData |>
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
  .x = unique(gaWebCommentData[["CommentID"]]),
  .f = \(commentID) {
    
    ### assign location nodes ----
    locationNodes <- gaGroundTruthLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    
    ### calculate comment similarity ----
    if (length(locationNodes) > 1) {
      commentSimilarity <- evaluateLocationGraphSimilarity(
        locationNodes = locationNodes,
        groundTruthRelationships = gaGroundTruthRelationships |>
          dplyr::filter(CommentID == commentID),
        comparisonRelationships = gaWebCommentRelationships |>
          dplyr::filter(CommentID == commentID)
      )
    } else {
      commentSimilarity <- NULL
    }
    
    ### return comment similarity ----
    return(commentSimilarity)
  }
) |>
  purrr::set_names(nm = unique(gaWebCommentData[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID")
