
# Script 01: Scrape Pennsylvania Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(rvest)
library(httr2)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Pennsylvania/"
paWebCommentsProcessedFilename <- "PAWebComments.rds"

# assign pennsylvania comments url ----
# 5856 comment submission pages
baseURL <- "https://redistricting.state.pa.us/comment/submission/"

# scrape web comments for pennsylvania ----
paWebComments <- purrr::map(
  .progress = TRUE,
  .x = 1:5856,
  .f = purrr::safely(.f = \(pageID) {
    Sys.sleep(time = 0.1)
    
    ## scrape page html ----
    pageHTML <- paste0(baseURL, pageID) |> scrapeWebComment()
    
    ## scrape commenter name ----
    commenterName <- pageHTML |>
      rvest::html_elements(css = "div.post-meta span") |>
      rvest::html_text2() |>
      stringr::str_remove(pattern = "By ")
    
    ## scrape comment data ----
    commentDate <- pageHTML |>
      rvest::html_elements(css = ".post-date.ml-0") |>
      rvest::html_text2() |>
      lubridate::dmy()
    
    ## scrape comment text ----
    commentText <- pageHTML |>
      rvest::html_elements(css = "p.mt-5.pre-line") |>
      rvest::html_text2()
    
    ## compile comment information ----
    webComment <- data.frame(
      Name = commenterName,
      Date = commentDate,
      Comment = commentText
    )
    
    ## return web comment ----
    return(webComment)
  })
)

# extract comment errors ----
paWebCommentsErrors <- paWebComments |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = paWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = paWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Page Count: {errorCount}")))

# extract valid results ----
paWebComments <- paWebComments |>
  purrr::map(.f = \(commentPage) commentPage$result) |>
  purrr::list_rbind() |>
  dplyr::distinct(Name, Date, Comment, .keep_all = TRUE)

# save processed colorado web comments ----
saveRDS(object = paWebComments, file = file.path(dataPath, paWebCommentsProcessedFilename))
