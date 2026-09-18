
# Script 04: Extract Requests from Georgia Web Comments

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
gaWebCommentsFilename <- "GAWebComments.rds"
gaWebCommentLocationsFilename <- "GAWebCommentLocations.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
gaWebCommentRelationshipsFilename <- "GAWebCommentRelationships.rds"

# import web comments ----
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename))

# import georgia comment data ----
gaWebCommentLocations <- readRDS(file = file.path(dataPath, gaWebCommentLocationsFilename))

# import georgia ground truth location names ----
gaWebCommentLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Georgia")

# evaluate location relationships for each comment ----
extractedWebCommentLocations <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = gaWebCommentLocations |> dplyr::pull(CommentID) |> unique(),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- gaWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractCommentRequests(
        comment = gaWebComments |> dplyr::filter(CommentID == commentID) |> dplyr::pull(Comment),
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
gaWebCommentsErrors <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
gaWebCommentRelationships <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(gaWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(CommentID) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = gaWebCommentRelationships, file = file.path(dataPath, gaWebCommentRelationshipsFilename))
