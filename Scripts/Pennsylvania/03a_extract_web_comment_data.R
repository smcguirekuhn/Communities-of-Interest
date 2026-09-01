
# Script 03a: Extract COI Information from Pennsylvania Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)
library(jsonlite)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Pennsylvania/"
paWebCommentsFilename <- "PAWebComments.rds"
paWebCommentDataFilename <- "PAWebCommentData.rds"
paGroundTruthFilename <- "PAGroundTruthCommentData.json"

# import georgia web comments and filter to coi comments ----
paWebComments <- readRDS(file = file.path(dataPath, paWebCommentsFilename))

# create sample of duplicates for ground truth coding ----
set.seed(seed = 1998)
sampledCommentIDs <- paWebComments |>
  dplyr::filter(COI == 1) |>
  dplyr::pull(CommentID) |>
  sample(size = 50, replace = FALSE)

# add comment screening column ----
paWebCommentData <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = paWebComments |> dplyr::filter(CommentID %in% sampledCommentIDs) |> dplyr::pull(Date) |> unique(),
  .f = purrr::safely(\(commentDay) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 1)
    
    ## isolate comments for an individual county ----
    dayWebComments <- paWebComments |>
      dplyr::filter(CommentID %in% sampledCommentIDs, Date == commentDay) |>
      dplyr::mutate(Characters = nchar(Comment))
    
    ## gather comment information ----
    commentInfo <- extractCommentInfo(
      prompts = dayWebComments |> dplyr::pull(Comment) |> as.list(),
      localContext = "Pennsylvania",
      adminLevels = c(
        "Landmark",
        "School",
        "Neighborhood",
        "Township",
        "Borough",
        "City",
        "School District",
        "County",
        "State House District",
        "State Senate District",
        "Congressional District",
        "Region",
        "NA"
      )
    )
    
    ## bind comment information ----
    dayWebComments <- dayWebComments |>
      dplyr::bind_cols(commentInfo)
    
    ## return comment information ----
    return(dayWebComments)
  })
)

# extract comment errors ----
paWebCommentsErrors <- paWebCommentData |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = paWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
paWebCommentData <- paWebCommentData |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::list_rbind() |>
  dplyr::rename(CommenterName = Name) |>
  tidyr::unnest(cols = "LocationsMentioned") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID) |>
  addFullLocationNames()

# save ground truth comment sets ----
paWebCommentData |>
  dplyr::relocate(Sentiment, Clarity, .before = "Name") |>
  dplyr::mutate(Reviewed = 0) |>
  tidyr::nest(LocationsMentioned = Name:FullLocationName) |>
  jsonlite::write_json(path = file.path(dataPath, paGroundTruthFilename), pretty = TRUE)

# save comment data ----
saveRDS(object = paWebCommentData, file = file.path(dataPath, paWebCommentDataFilename))
