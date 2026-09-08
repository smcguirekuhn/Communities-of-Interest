
# Script 03: Extract COI Information from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentsFilename <- "COWebComments.rds"
coWebCommentDataFilename <- "COWebCommentData.rds"

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
  "Denver Metro Area",
  "Rural Colorado"
)

# create sample of duplicates for ground truth coding ----
set.seed(seed = 1998)
sampledCommentIDs <- coWebComments |>
  dplyr::filter(COI == 1) |>
  dplyr::pull(CommentID) |>
  sample(size = 50, replace = FALSE)

# add comment information columns ----
coWebCommentData <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = coWebComments |> dplyr::filter(CommentID %in% sampledCommentIDs) |> dplyr::pull(ZIPCode) |> unique(),
  .f = purrr::safely(\(commentZIPCode) {
    Sys.sleep(time = 1)
    
    ## isolate comments for an individual zip code ----
    zipCodeWebComments <- coWebComments |>
      dplyr::filter(CommentID %in% sampledCommentIDs, ZIPCode == commentZIPCode) |>
      dplyr::mutate(Characters = nchar(Comment))
    
    ## gather comment information ----
    commentInfo <- extractCommentInfo(
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

# extract comment errors ----
coWebCommentsErrors <- coWebCommentData |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = coWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = coWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
coWebCommentData <- coWebCommentData |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::list_rbind() |>
  dplyr::rename(CommenterName = Name) |>
  tidyr::unnest(cols = "LocationsMentioned") |>
  dplyr::filter(Name != "NA") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID) |>
  addFullLocationNames()

# save comment data ----
saveRDS(object = coWebCommentData, file = file.path(dataPath, coWebCommentDataFilename))
