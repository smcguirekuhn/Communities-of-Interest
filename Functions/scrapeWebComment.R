
scrapeWebComment <- function(commentPageURL) {
  
  # scrape web comment ----
  tryCatch(
    expr = {
      
      ## make html request ----
      scrapeRequest <- httr2::request(commentPageURL) |>
        httr2::req_user_agent("COIResearchCrawler (smk7761@psu.edu)") |>
        httr2::req_throttle(rate = 1) |>
        httr2::req_retry(max_tries = 5, backoff = ~ 2^.x)
      
      ## perform html request ----
      scrapeResponse <- scrapeRequest |>
        httr2::req_perform() |>
        httr2::resp_body_html()
      
      ## return response ----
      return(scrapeResponse)
    },
    error = function(e) {
      
      ## return error message ----
      cli::cli_inform(message = glue::glue("Failed to fetch {commentPageURL}, Error: {e$message}"))
      return(NULL)
    }
  )
}