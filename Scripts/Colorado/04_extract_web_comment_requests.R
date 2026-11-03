
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
coWebCommentsFilename <- "COWebComments.rds"
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"
coWebCommentRequestsFilename <- "COWebCommentRequests.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"

# import web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# import colorado comment data ----
coWebCommentLocations <- readRDS(file = file.path(dataPath, coWebCommentLocationsFilename))

# evaluate location requests for each comment ----
extractedWebCommentRequests <- purrr::map(
  .progress = "Evaluating Location Requests",
  .x = coWebCommentLocations |> dplyr::pull(CommentID) |> unique(), # missed comment 1014
  .f = purrr::safely(.f = \(commentID) {
    
    ## assign location names ----
    locationNames <- coWebCommentLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName) |>
      unique()
    
    ## evaluate comment location requests ----
    commentRequests <- extractCommentRequests(
      comment = coWebComments |> dplyr::filter(CommentID == commentID) |> dplyr::pull(Comment),
      locationNames = locationNames,
      state = "Colorado"
    )
    
    ## return comment location requests ----
    Sys.sleep(time = 0.5)
    return(commentRequests)
  })
)

# extract comment errors ----
coWebCommentsErrors <- extractedWebCommentRequests |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = coWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = coWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat ----
coWebCommentRequests <- extractedWebCommentRequests |>
  purrr::map(
    .f = \(webComment) {
      dplyr::bind_rows(
        dplyr::bind_cols(webComment$result$PairwiseGroups, dplyr::tibble(Request = "grouped")),
        dplyr::bind_cols(webComment$result$PairwiseSeparations, dplyr::tibble(Request = "separated")),
        dplyr::bind_cols(
          dplyr::tibble(Location1 = webComment$result$LocationsKeptWhole),
          dplyr::tibble(Location2 = webComment$result$LocationsKeptWhole),
          dplyr::tibble(Request = "kept whole")
        )
      )
    }
  ) |>
  purrr::set_names(nm = unique(coWebCommentLocations[["CommentID"]])) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID) |>
  dplyr::distinct() |>
  tidyr::drop_na() |>
  dplyr::mutate(State = "Colorado", .before = 1)

# reconcile request transitivity ----
coAdjustedWebCommentRequests <- purrr::map_dfr(
  .progress = "Reconciling request transitivity",
  .x = coWebCommentLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    
    ## create location graphs ----
    locationGraphs <- createLocationGraphs(
      locationNodes = coWebCommentRequests |>
        dplyr::filter(CommentID == commentID) |>
        tidyr::pivot_longer(cols = Location1:Location2, names_to = "Location", values_to = "Name") |>
        dplyr::pull(name = Name) |>
        as.character() |>
        unique(),
      requests = coWebCommentRequests |> dplyr::filter(CommentID == commentID)
    )
    
    ## extract location graph edges ----
    locationGraphEdges <- dplyr::bind_rows(
      igraph::as_data_frame(locationGraphs[["GroupedGraph"]], what = "edges") |>
        dplyr::mutate(
          Location1 = pmin(from, to),
          Location2 = pmax(from, to),
          Request = "grouped",
          .keep = "unused"
        ),
      igraph::as_data_frame(locationGraphs[["SeparatedGraph"]], what = "edges") |>
        dplyr::mutate(
          Location1 = pmin(from, to),
          Location2 = pmax(from, to),
          Request = "separated",
          .keep = "unused"
        )
    ) |> dplyr::mutate(State = "Colorado", CommentID = commentID, .before = 1)
    
    ## return location graph edges ----
    return(locationGraphEdges)
  }
)

# add kept whole requests back into the reconciled set ----
coAdjustedWebCommentRequests <- coAdjustedWebCommentRequests |>
  dplyr::bind_rows(coWebCommentRequests |> dplyr::filter(Request == "kept whole")) |>
  dplyr::arrange(CommentID)

# save comment requests ----
saveRDS(object = coAdjustedWebCommentRequests, file = file.path(dataPath, coWebCommentRequestsFilename))
