
# Script 04: Extract Requests from Colorado Web Comments

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
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"
coWebCommentRelationshipsFilename <- "COWebCommentRelationships.rds"

# import colorado comment data ----
coWebCommentLocations <- readRDS(file = file.path(dataPath, coWebCommentLocationsFilename))

# evaluate location relationships for each comment ----
coWebCommentRelationships <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = unique(coWebCommentLocations[["CommentID"]]),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- coWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractCommentRequests(
        comment = coWebCommentLocations |>
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
  purrr::set_names(nm = unique(coWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(as.numeric(CommentID)) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = coWebCommentRelationships, file = file.path(dataPath, coWebCommentRelationshipsFilename))
