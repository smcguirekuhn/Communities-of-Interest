
# Script 04: Extract Location Relationships from Georgia Web Comments

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
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"
gaWebCommentRelationshipsFilename <- "GAWebCommentRelationships.rds"

# import georgia comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# evaluate location relationships for each comment ----
gaWebCommentRelationships <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = unique(gaWebCommentData[["CommentID"]]),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- gaGroundTruthCommentData |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractLocationRelationships(
        comment = gaWebCommentData |>
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
gaWebCommentsErrors <- gaWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
gaWebCommentRelationships <- gaWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(gaWebCommentData[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(as.numeric(CommentID)) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = gaWebCommentRelationships, file = file.path(dataPath, gaWebCommentRelationshipsFilename))
