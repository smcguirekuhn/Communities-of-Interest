
# Script 03: Extract Locations from Georgia Web Comments

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
dataPath <- "./Data/Georgia/"
gaWebCommentsFilename <- "GAWebComments.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
gaWebCommentLocationsFilename <- "GAWebCommentLocations.rds"

# import web comments ----
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename))

# assign relevant state regions ----
stateRegions <- c(
  "Northern Georgia",
  "Northeastern Georgia",
  "Eastern Georgia",
  "Southeastern Georgia",
  "Southern Georgia",
  "Southwestern Georgia",
  "Western Georgia",
  "Northwestern Georgia",
  "Atlanta Metro Area",
  "Rural Georgia",
  "Coastal Georgia"
)

# assign comment ids to extract locations for ----
sampledCommentIDs <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Georgia") |>
  dplyr::pull(CommentID) |>
  unique()

# add comment information columns ----
extractedWebCommentLocations <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = gaWebComments |> dplyr::filter(CommentID %in% sampledCommentIDs) |> dplyr::pull(County) |> unique(),
  .f = purrr::safely(\(county) {
    
    ## isolate comments for an individual county ----
    countyWebComments <- gaWebComments |>
      dplyr::filter(CommentID %in% sampledCommentIDs, County == county) |>
      dplyr::select(CommentID, Comment) |>
      dplyr::slice(rep(x = 1:dplyr::n(), each = 3)) |>
      dplyr::mutate(Iteration = rep(x = 1:3, dplyr::n()/3))
    
    ## gather comment information ----
    commentInfo <- extractCommentLocations(
      prompts = countyWebComments |> dplyr::pull(Comment) |> as.list(),
      localContext = glue::glue("{county}, Georgia"),
      stateRegions = stateRegions
    )
    
    ## bind comment information ----
    countyWebComments <- countyWebComments |>
      dplyr::bind_cols(commentInfo)
    
    ## return comment information ----
    Sys.sleep(time = 3)
    return(countyWebComments)
  })
)

# evaluate extraction errors ----
extractionErrors <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = extractionErrors, FUN = is.null))
errorIDs <- which(!sapply(X = extractionErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Extraction Errors: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
gaWebCommentLocations <- extractedWebCommentLocations |>
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
saveRDS(object = gaWebCommentLocations, file = file.path(dataPath, gaWebCommentLocationsFilename))
