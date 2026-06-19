
# reset global environment ----
rm(list = ls())

# import packages ----
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# assign import and export destinations ----
dataPath <- "./Data/"
gaWebCommentsRawFilename <- "GA_Web_Comments_030922.txt"
gaWebCommentsProcessedFilename <- "GAWebComments.rds"

# clean and process georgia web comments ----
gaWebComments <- readr::read_lines(file = file.path(dataPath, gaWebCommentsRawFilename))[-1] |>
  dplyr::tibble(Line = _) |>
  dplyr::mutate(CommentID = cumsum(Line == "")) |>
  dplyr::filter(Line != "") |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(
    Header = dplyr::first(Line),
    Comment = ifelse(
      test = dplyr::n() > 1,
      yes = paste(Line[-1], collapse = " "),
      no = NA
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Header = Header |>
      stringr::str_trim() |>
      stringr::str_replace(pattern = '^"(.*)"$', replacement = '\\1'),
    Comment = Comment |>
      stringr::str_trim() |>
      stringr::str_replace(pattern = '^"(.*)"$', replacement = '\\1')
  ) |>
  dplyr::mutate(
    Date = Header |> stringr::str_extract(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}") |> lubridate::mdy(),
    Header = Header |> stringr::str_remove(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}"),
    Name = Header |> stringr::str_extract(pattern = "^.+(?=\\s+of\\s+)") |> stringr::str_to_lower(),
    County = Header |> stringr::str_extract(pattern = "(?<=\\sof\\s).+$"),
    .before = Comment
  ) |>
  dplyr::select(-Header) |>
  tidyr::drop_na(Comment)

# save processed georgia web comments ----
saveRDS(object = gaWebComments, file = file.path(dataPath, gaWebCommentsProcessedFilename))

## initialize chat object ----
chat <- ellmer::chat_openrouter(model = "mistralai/mistral-large")

## gather individual comment data and mentioned locations ----
chatOutput <- chat$chat_structured(
  gaWebComments$Comment[268],
  type = ellmer::type_integer(
    description = paste(
      "Binary indicator of whether this Georgia commenter is",
      "discussing a redistricting community of interest.",
      "Return 1 if the commenter is discussing a community of interest,",
      "and return 0 if the commenter is discussing a different redistricting concern or otherwise."
    )
  )
)
