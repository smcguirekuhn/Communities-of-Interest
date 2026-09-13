
# Script 03: Extract COI Information from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentsFilename <- "COWebComments.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"

# import web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# assign relevant state regions ----
stateRegions <- c(
  "Northern Colorado",
  "Northeastern Colorado",
  "Eastern Colorado",
  "Southeastern Colorado",
  "Southern Colorado",
  "Southwestern Colorado",
  "Western Colorado",
  "Northwestern Colorado",
  "Western Slope",
  "Front Range",
  "Eastern Plains",
  "San Luis Valley",
  "Arkansas Valley",
  "Denver Metro Area",
  "Rural Colorado"
)

# assign comment ids to extract locations for ----
sampledCommentIDs <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Colorado") |>
  dplyr::pull(CommentID) |>
  unique()

# add comment information columns ----
extractedWebCommentLocations <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = coWebComments |> dplyr::filter(CommentID %in% sampledCommentIDs) |> dplyr::pull(ZIPCode) |> unique(),
  .f = purrr::safely(\(commentZIPCode) {
    Sys.sleep(time = 1)
    
    ## isolate comments for an individual zip code ----
    zipCodeWebComments <- coWebComments |>
      dplyr::filter(CommentID %in% sampledCommentIDs, ZIPCode == commentZIPCode) |>
      dplyr::select(CommentID, Comment) |>
      dplyr::slice(rep(x = 1:dplyr::n(), each = 5)) |>
      dplyr::mutate(Iteration = rep(x = 1:5, dplyr::n()/5))
    
    ## gather comment information ----
    commentInfo <- extractCommentLocations(
      prompts = zipCodeWebComments |> dplyr::pull(Comment) |> as.list(),
      localContext = glue::glue("ZIP Code {commentZIPCode} in Colorado"),
      stateRegions = stateRegions
    )
    
    ## bind comment information ----
    zipCodeWebComments <- zipCodeWebComments |>
      dplyr::bind_cols(commentInfo)
    
    ## return comment information ----
    return(zipCodeWebComments)
  })
)

# evaluate extraction errors ----
extractionErrors <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = extractionErrors, FUN = is.null))
errorIDs <- which(!sapply(X = extractionErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Extraction Errors: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
coWebCommentLocations <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::list_rbind() |>
  dplyr::select(-Comment) |>
  tidyr::unnest(cols = "LocationsMentioned") |>
  dplyr::filter(Name != "NA") |>
  dplyr::arrange(CommentID) |>
  addFullLocationNames() |>
  dplyr::distinct(CommentID, Iteration, FullLocationName, .keep_all = TRUE) |>
  dplyr::group_by(CommentID, FullLocationName) |>
  dplyr::mutate(Frequency = dplyr::n(), ContextualFrequency = sum(Relevance == "contextual")) |>
  dplyr::ungroup()

# save comment data ----
saveRDS(object = coWebCommentLocations, file = file.path(dataPath, coWebCommentLocationsFilename))
