
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
gaWebCommentRequestsFilename <- "GAWebCommentRequests.rds"

# import web comments ----
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename))

# import georgia comment data ----
gaWebCommentLocations <- readRDS(file = file.path(dataPath, gaWebCommentLocationsFilename))

# import georgia ground truth location names ----
gaWebCommentLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Georgia")

# evaluate location requests for each comment ----
extractedWebCommentRequests <- purrr::map(
  .progress = "Evaluating Location Requests",
  .x = gaWebCommentLocations |> dplyr::pull(CommentID) |> unique(),
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- gaWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location requests ----
    if (length(locationNames) > 1) {
      commentRequests <- extractCommentRequests(
        comment = gaWebComments |> dplyr::filter(CommentID == commentID) |> dplyr::pull(Comment),
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
gaWebCommentsErrors <- extractedWebCommentRequests |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
gaWebCommentRequests <- extractedWebCommentRequests |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::set_names(nm = unique(gaWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID) |>
  dplyr::distinct() |>
  dplyr::mutate(State = "Georgia", .before = 1)

# save comment requests ----
saveRDS(object = gaWebCommentRequests, file = file.path(dataPath, gaWebCommentRequestsFilename))
