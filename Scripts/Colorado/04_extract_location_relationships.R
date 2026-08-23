
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
coWebCommentRelationshipsFilename <- "COWebCommentRelationships.rds"

# import colorado comment data ----
coWebCommentData <- readRDS(file = file.path(dataPath, coWebCommentDataFilename))

# evaluate location relationships for each comment ----
coWebCommentRelationships <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = unique(coWebCommentData[["CommentID"]]),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- coWebCommentData |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractLocationRelationships(
        comment = coWebCommentData |>
          dplyr::filter(CommentID == commentID) |>
          dplyr::pull(Comment) |>
          unique(),
        locationNames = locationNames
      )
    } else {
      commentRelationships <- NULL
    }
    
    ## return comment location relationships ----
    return(commentRelationships)
  })
)

# extract comment errors ----
coWebCommentsErrors <- coWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = coWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = coWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
coWebCommentRelationships <- coWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(coWebCommentData[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(as.numeric(CommentID)) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = coWebCommentRelationships, file = file.path(dataPath, coWebCommentRelationshipsFilename))
