
# Script 04: Extract Location Relationships from Pennsylvania Web Comments

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
paWebCommentDataFilename <- "PAWebCommentData.rds"
paGroundTruthFilename <- "PAGroundTruthCommentData.json"
paWebCommentRelationshipsFilename <- "PAWebCommentRelationships.rds"

# import pennsylvania comment data ----
paWebCommentData <- readRDS(file = file.path(dataPath, paWebCommentDataFilename))

# evaluate location relationships for each comment ----
paWebCommentRelationships <- purrr::map(
  .progress = "Evaluating Location Relationships",
  .x = unique(paWebCommentData[["CommentID"]]),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- paWebCommentData |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    
    ## evaluate comment location relationships ----
    if (length(locationNames) > 1) {
      commentRelationships <- extractLocationRelationships(
        comment = paWebCommentData |>
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
paWebCommentsErrors <- paWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = paWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
paWebCommentRelationships <- paWebCommentRelationships |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(paWebCommentData[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::arrange(as.numeric(CommentID)) |>
  dplyr::distinct()

# save comment relationships ----
saveRDS(object = paWebCommentRelationships, file = file.path(dataPath, paWebCommentRelationshipsFilename))
