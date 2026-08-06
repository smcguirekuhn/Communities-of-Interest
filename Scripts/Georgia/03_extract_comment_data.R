
# Script 03: Extract COI Information from Georgia Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(ellmer)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia/"
gaWebCommentsFilename <- "GAWebComments.rds"
gaWebCommentDataFilename <- "GAWebCommentData.rds"

# import georgia web comments and filter to coi comments ----
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename))

# add comment screening column ----
gaWebCommentData <- purrr::map(
  .progress = "Extracting Comment Information",
  .x = unique(gaWebComments[["County"]]),
  .f = purrr::safely(\(county) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 1)
    
    ## isolate comments for an individual county ----
    countyWebComments <- gaWebComments |>
      dplyr::filter(COI == 1, County == county) |>
      dplyr::mutate(Characters = nchar(Comment))
    
    ## define local context for comment data extraction ----
    localContext <- glue::glue("{county}, Georgia")
    
    ## gather comment information ----
    commentInfo <- extractCommentInfo(
      prompts = countyWebComments |> dplyr::pull(Comment) |> as.list(),
      localContext = localContext,
      adminLevels = c(
        "Landmark",
        "School",
        "Neighborhood",
        "Town",
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
    countyWebComments <- countyWebComments |>
      dplyr::bind_cols(commentInfo)
    
    ## return comment information ----
    return(countyWebComments)
  })
)

# extract comment errors ----
gaWebCommentsErrors <- gaWebCommentData |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results and reformat on a location-wise basis ----
gaWebCommentData <- gaWebCommentData |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::list_rbind() |>
  dplyr::rename(CommenterName = Name) |>
  tidyr::unnest(cols = "LocationsMentioned") |>
  dplyr::arrange(as.numeric(CommentID))

# # create sample of duplicates for ground truth coding ----
# set.seed(seed = 1998)
# gaWebCommentData[sample(x = 1:length(gaWebCommentData), size = 50, replace = FALSE)] |>
#   saveRDS(file = file.path(dataPath, groundTruthFileName))

# save comment data ----
saveRDS(object = gaWebCommentData, file = file.path(dataPath, gaWebCommentDataFilename))
