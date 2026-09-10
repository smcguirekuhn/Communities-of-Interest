
# Script 02: Screen Georgia Web Comments for Communities of Interest Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia/"
gaWebCommentsFilename <- "GAWebComments.rds"

# import georgia web comments ----
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename))

# add comment screening column ----
gaCOIWebComments <- purrr::map(
  .progress = TRUE,
  .x = 1:nrow(gaWebComments),
  .f = purrr::safely(\(webCommentID) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 1)
    
    ## add screening column to individual comment ----
    webCommentRow <- gaWebComments |>
      dplyr::slice(webCommentID) |>
      dplyr::mutate(COI = screenCommentForCOI(description = Comment, state = "Georgia"))
    
    ## return comment information ----
    return(webCommentRow)
  })
)

# extract comment errors ----
gaCOIWebCommentsErrors <- gaCOIWebComments |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaCOIWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaCOIWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results ----
gaCOIWebComments <- gaCOIWebComments |>
  purrr::map(.f = \(webComment) webComment$result) |>
  purrr::list_rbind()

# save comment data ----
saveRDS(object = gaCOIWebComments, file = file.path(dataPath, gaWebCommentsFilename))
