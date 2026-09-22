
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
paWebCommentRequestsFilename <- "PAWebCommentRequests.rds"

# import web comments ----
paWebComments <- readRDS(file = file.path(dataPath, paWebCommentsFilename))

# import pennsylvania comment data ----
paWebCommentLocations <- readRDS(file = file.path(dataPath, paWebCommentLocationsFilename))

# import pennsylvania ground truth location names ----
paWebCommentLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Pennsylvania")

# evaluate location requests for each comment ----
extractedWebCommentRequests <- purrr::map(
  .progress = "Evaluating Location Requests",
  .x = paWebCommentLocations |> dplyr::pull(CommentID) |> unique(),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- paWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location requests ----
    if (length(locationNames) > 1) {
      commentRequests <- extractCommentRequests(
        comment = paWebComments |> dplyr::filter(CommentID == commentID) |> dplyr::pull(Comment),
        locationNames = locationNames
      )
    } else {
      commentRequests <- NULL
    }
    
    ## return comment location requests ----
    Sys.sleep(time = 3)
    return(commentRequests)
  })
)

# extract comment errors ----
paWebCommentsErrors <- extractedWebCommentRequests |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = paWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
paWebCommentRequests <- extractedWebCommentRequests |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(paWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID) |>
  dplyr::distinct()

# save comment requests ----
saveRDS(object = paWebCommentRequests, file = file.path(dataPath, paWebCommentRequestsFilename))
