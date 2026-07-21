
# Script 01: Clean Georgia Web Comments Data

# reset global environment ----
rm(list = ls())

# import packages ----
library(rvest)
library(purrr)
library(dplyr)
library(stringr)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentsProcessedFilename <- "COWebComments.rds"

# assign colorado comments url ----
# 601 pages of archived comments, roughly 6006 comments
pageURL <- "/web/20210713125233/https://redistricting.colorado.gov/public_comments/"
nextURL <- pageURL

# scrape web comments for colorado ----
coWebComments <- purrr::map(
  .progress = TRUE,
  .x = 1:5,
  .f = purrr::safely(.f = \(pageID) {
    Sys.sleep(time = 1)
    
    ## scrape page html ----
    pageHTML <- paste0("https://web.archive.org", nextURL) |> rvest::read_html()
    
    ## scrape comment card elements ----
    commentElements <- pageHTML |>
      rvest::html_elements(css = ".dashboard-wrapper") |>
      tail(-1)
    
    ## gather commenter names ----
    commenterNames <- commentElements |>
      rvest::html_elements(css = "h4") |>
      rvest::html_text2()
    
    ## gather comment commission types ----
    commenterCommissionType <- commentElements |>
      rvest::html_elements(css = "p:nth-of-type(1)") |>
      rvest::html_text2() |>
      stringr::str_remove(pattern = "Commission: ")
    
    ## gather commenter zip codes ----
    commenterZIPCodes <- commentElements |>
      rvest::html_elements(css = "p:nth-of-type(2)") |>
      rvest::html_text2() |>
      stringr::str_remove(pattern = "Zip: ")
    
    ## gather comment dates ----
    commenterDate <- commentElements |>
      rvest::html_elements(css = "p:nth-of-type(3)") |>
      rvest::html_text2() |>
      stringr::str_remove(pattern = "Submittted: ")
    
    ## gather comment texts ----
    commentText <- commentElements |>
      rvest::html_elements(css = ".comment") |>
      rvest::html_text2()
    
    ## compile comment information ----
    pageComments <- data.frame(
      Name = commenterNames,
      Commission = commenterCommissionType,
      ZIPCode = commenterZIPCodes,
      Date = commenterDate,
      Comment = commentText
    )
    
    ## establish next page url ----
    nextURL <<- paste0("https://web.archive.org", nextURL) |>
      rvest::read_html() |>
      rvest::html_element(css = "a[rel='next']") |>
      rvest::html_attr("href")
    
    ## return compiled comments ----
    return(pageComments)
  })
)

# extract comment errors ----
coWebCommentsErrors <- coWebComments |>
  purrr::map(.f = \(webComment) webComment$error)
errorCount <- sum(!sapply(X = coWebCommentsErrors, FUN = is.null))
errorIDs <- which(!sapply(X = coWebCommentsErrors, FUN = is.null))
cli::cli_inform(message = c(">" = glue::glue("Erroneous Comment Page Count: {errorCount}")))

# extract valid results ----
coWebComments <- coWebComments |>
  purrr::map(.f = \(commentPage) commentPage$result) |>
  purrr::list_rbind()

# save processed georgia web comments ----
saveRDS(object = coWebComments, file = file.path(dataPath, coWebCommentsProcessedFilename))
