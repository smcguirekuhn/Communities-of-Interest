
# Script 03a: Extract COI Information from Pennsylvania Web Comments

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
dataPath <- "./Data/Pennsylvania/"
paWebCommentsFilename <- "PAWebComments.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
paWebCommentLocationsFilename <- "PAWebCommentLocations.rds"

# import web comments ----
paWebComments <- readRDS(file = file.path(dataPath, paWebCommentsFilename))

# assign relevant state regions ----
stateRegions <- c(
  "Northern Pennsylvania",
  "Northeastern Pennsylvania",
  "Eastern Pennsylvania",
  "Southeastern Pennsylvania",
  "Southern Pennsylvania",
  "Southwestern Pennsylvania",
  "Western Pennsylvania",
  "Northwestern Pennsylvania",
  "Pennsylvania Wilds",
  "The Poconos",
  "Happy Valley",
  "Lehigh Valley",
  "Coal Region",
  "Mon Valley",
  "Philadelphia Metro Area",
  "Pittsburgh Metro Area"
)

# assign comment ids to extract locations for ----
sampledCommentIDs <- readRDS(file = file.path(allGroundTruthLocationsFilename)) |>
  dplyr::filter(State == "Pennsylvania") |>
  dplyr::pull(CommentID) |>
  unique()

# add comment information columns ----
extractedWebCommentLocations <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = paWebComments |> dplyr::filter(CommentID %in% sampledCommentIDs) |> dplyr::pull(Date) |> unique(),
  .f = purrr::safely(\(commentDay) {
    Sys.sleep(time = 1)
    
    ## isolate comments for an individual day ----
    dayWebComments <- paWebComments |>
      dplyr::filter(CommentID %in% sampledCommentIDs, Date == commentDay) |>
      dplyr::select(CommentID, Comment) |>
      dplyr::slice(rep(x = 1:dplyr::n(), each = 5)) |>
      dplyr::mutate(Iteration = rep(x = 1:5, dplyr::n()/5))
    
    ## gather comment information ----
    commentInfo <- extractCommentLocations(
      prompts = dayWebComments |> dplyr::pull(Comment) |> as.list(),
      localContext = "Pennsylvania",
      stateRegions = stateRegions
    )
    
    ## bind comment information ----
    dayWebComments <- dayWebComments |>
      dplyr::bind_cols(commentInfo)
    
    ## return comment information ----
    return(dayWebComments)
  })
)

# evaluate extraction errors ----
extractionErrors <- extractedWebCommentLocations |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = extractionErrors, FUN = is.null))
errorIDs <- which(!sapply(X = extractionErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Extraction Errors: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
paWebCommentLocations <- extractedWebCommentLocations |>
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
saveRDS(object = paWebCommentLocations, file = file.path(dataPath, paWebCommentLocationsFilename))
