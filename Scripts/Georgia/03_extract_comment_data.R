
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
gaWebComments <- readRDS(file = file.path(dataPath, gaWebCommentsFilename)) |>
  dplyr::filter(COI == 1)

# add comment screening column ----
gaWebCommentData <- purrr::map(
  .progress = TRUE,
  .x = 1:nrow(gaWebComments),
  .f = purrr::safely(\(webCommentID) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 1)
    
    ## isolate individual comment ----
    webComment <- gaWebComments |>
      dplyr::slice(webCommentID) |>
      dplyr::mutate(Characters = nchar(Comment)) |>
      as.list()
    
    ## gather comment information ----
    commentInfo <- extractCommentInfo(
      description = webComment$Comment,
      state = "Georgia",
      adminLevels = c("Neighborhood", "Town", "City", "SchoolDistrict", "County", "Region")
    )
    
    ## append comment information ----
    webComment <- webComment |> append(values = commentInfo)
    
    ## return comment information ----
    return(webComment)
  })
)

# extract comment errors ----
gaWebCommentsErrors <- gaWebCommentData |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = gaWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = gaWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Count: {errorCount}")))

# extract valid results ----
gaWebCommentData <- gaWebCommentData |> purrr::map(.f = \(webComment) webComment$result)

# simplify location administrative levels ----
gaWebCommentData <- gaWebCommentData |>
  purrr::map(
    .f = \(commentInfo) {
      
      ## assigned locations mentioned ----
      LocationsMentioned <- commentInfo$LocationsMentioned
      
      ## find designated administrative level ----
      if (nrow(LocationsMentioned) > 0) {
        adminLevels <- 1:nrow(LocationsMentioned) |>
          purrr::map(
            .f = \(locationID) {
              adminLevel <- LocationsMentioned$AdminLevel |>
                dplyr::slice(locationID) |>
                dplyr::select(dplyr::where(~any(.x == 1))) |>
                names()
              if (length(adminLevel) == 0) adminLevel <- "other"
              return(adminLevel)
            }
          )
      } else {
        adminLevels <- list()
      }
      
      ## format designated administrative level ----
      LocationsMentioned$AdminLevel <- adminLevels
      
      ## update comment information ----
      commentInfo$LocationsMentioned <- LocationsMentioned
      
      ## return comment information ----
      return(commentInfo)
    }
  )

# save comment data ----
saveRDS(object = gaWebCommentData, file = file.path(dataPath, gaWebCommentDataFilename))
