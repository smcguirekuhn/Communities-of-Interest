
# Script 04: Extract Requests from Pennsylvania Web Comments

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
dataPath <- "./Data/Pennsylvania/"
paWebCommentsFilename <- "PAWebComments.rds"
paWebCommentLocationsFilename <- "PAWebCommentLocations.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
paWebCommentRelationshipsFilename <- "PAWebCommentRelationships.rds"

# import web comments ----
paWebComments <- readRDS(file = file.path(dataPath, paWebCommentsFilename))

# import pennsylvania comment data ----
paWebCommentLocations <- readRDS(file = file.path(dataPath, paWebCommentLocationsFilename))

# import pennsylvania ground truth location names ----
paWebCommentLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Pennsylvania")

# evaluate location relationships for each comment ----
extractedWebCommentLocations <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = paWebCommentLocations |> dplyr::pull(CommentID) |> unique(),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- paWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractCommentRequests(
        comment = paWebComments |> dplyr::filter(CommentID == commentID) |> dplyr::pull(Comment),
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
paWebCommentsErrors <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = paWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
paWebCommentRelationships <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(paWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(CommentID) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = paWebCommentRelationships, file = file.path(dataPath, paWebCommentRelationshipsFilename))
