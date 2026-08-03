
# Script 03b: Extract COI Information from Pennsylvania House Comments Data

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
dataPath <- "./Data/Pennsylvania"
paHouseCommentsFilename <- "PAHouseComments.rds"
paHouseCommentDataFilename <- "PAHouseCommentData.rds"
handCodingFilename <- "HandCodedCommentsPre.json"

# import pennsylvania house comments ----
paHouseComments <- readRDS(file = file.path(dataPath, paHouseCommentsFilename))

# add comment screening column ----
paHouseCommentData <- purrr::map(
  .progress = TRUE,
  .x = 1:nrow(paHouseComments),
  .f = purrr::safely(\(houseCommentID) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 1)
    
    ## isolate individual comment ----
    houseComment <- paHouseComments |>
      dplyr::slice(houseCommentID) |>
      dplyr::mutate(Characters = nchar(Comment)) |>
      as.list()
    
    ## gather comment information ----
    commentInfo <- extractCommentInfo(
      description = houseComment$Comment,
      state = "Pennsylvania",
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
    
    ## append comment information ----
    houseComment <- houseComment |> append(values = commentInfo)
    
    ## return comment information ----
    return(houseComment)
  })
)

# extract comment errors ----
paHouseCommentsErrors <- paHouseCommentData |>
  purrr::map(.f = \(houseComment) houseComment$error)
errorCount <- sum(!sapply(X = paHouseCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paHouseCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results ----
paHouseCommentData <- paHouseCommentData |> purrr::map(.f = \(houseComment) houseComment$result)

# save comment data ----
saveRDS(object = paHouseCommentData, file = file.path(dataPath, paHouseCommentDataFilename))

# create empty json file for hand-coding comments ----
set.seed(seed = 1998)
commentsSample <- purrr::map(
  .x = sample(1:length(paHouseCommentData), size = 15, replace = FALSE),
  .f = \(commentID) {
    commentInfo <- paHouseCommentData[[commentID]]
    commentInfo$LocationsMentioned <- commentInfo$LocationsMentioned |> dplyr::filter(FALSE)
    commentInfo$Sentiment <- NA
    commentInfo$Clarity <- NA
    commentInfo$ID <- commentID
    return(commentInfo)
  }
) |> jsonlite::write_json(path = file.path(dataPath, handCodingFilename), pretty = TRUE)
