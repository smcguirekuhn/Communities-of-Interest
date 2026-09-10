
# Validation Tests for Ellmer Pipeline Scripts Across All States

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(jsonlite)
library(igraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
groundTruthDataPath <- "./Validation/GroundTruth/"
ellmerOutputDataPath <- "./Validation/EllmerOutput/"
tablePath <- "./Tables/Validation/"
coGroundTruthFilename <- "COGroundTruthCommentData.json"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"

# import colorado ground truth data ----
coGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, coGroundTruthFilename),
  simplifyVector = TRUE
)

# format colorado ground truth locations ----
coGroundTruthLocations <- coGroundTruthData |>
  dplyr::select(CommentID, LocationsMentioned) |>
  tidyr::unnest(cols = LocationsMentioned) |>
  dplyr::mutate(
    SubareaDescription = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
      AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
      .default = "NA"
    ),
    .before = "FullLocationName"
  ) |>
  dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
  dplyr::mutate(State = "Colorado", .before = "CommentID")

# import georgia ground truth data ----
gaGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, gaGroundTruthFilename),
  simplifyVector = TRUE
)

# format georgia ground truth locations ----
gaGroundTruthLocations <- gaGroundTruthData |>
  dplyr::select(CommentID, LocationsMentioned) |>
  dplyr::mutate(CommentID = as.integer(CommentID)) |>
  tidyr::unnest(cols = LocationsMentioned) |>
  dplyr::mutate(
    SubareaDescription = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
      AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
      .default = "NA"
    ),
    .before = "FullLocationName"
  ) |>
  dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
  dplyr::mutate(State = "Georgia", .before = "CommentID")

# combine all ground truth locations data ----
allGroundTruthLocations <- dplyr::bind_rows(
  coGroundTruthLocations,
  gaGroundTruthLocations
)

# save all ground truth locations data ----
saveRDS(allGroundTruthLocations, file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# location recognition accuracy ----

## isolate colorado ground truth location mentions ----
coGroundTruthLocations <- coGroundTruthCommentData |>
  purrr::map(.f = \(webComment) webComment |> purrr::discard_at(at = "Relationships")) |>
  dplyr::bind_rows() |>
  tidyr::unnest_wider(col = "LocationsMentioned")

# evaluate location recognition across iteration numbers ----
iterationAccuracy <- purrr::map_dfr(
  .x = 1:5,
  .f = \(iteration) {
    evaluateLocationRecognition(
      groundTruthCommentData = coGroundTruthLocations,
      comparisonCommentData = coWebCommentData |> dplyr::filter(Iteration == iteration)
    ) |> dplyr::mutate(`Iteration` = iteration, .before = 1)
  }
)

# save iteration accuracy table ----
saveRDS(object = iterationAccuracy, file = file.path(tablePath, iterationAccuracyTableName))

# evaluate location recognition across frequency thresholds ----
frequencyAccuracy <- purrr::map_dfr(
  .x = 3:5,
  .f = \(threshold) {
    evaluateLocationRecognition(
      groundTruthCommentData = coGroundTruthLocations,
      comparisonCommentData = coWebCommentData |>
        dplyr::distinct(CommentID, Frequency, ContextualFrequency) |>
        dplyr::filter(Frequency >= threshold)
    ) |> dplyr::mutate(`Frequency Threshold` = threshold, .before = 1)
  }
)

# save frequency accuracy table ----
saveRDS(object = frequencyAccuracy, file = file.path(tablePath, frequencyAccuracyTableName))

# evaluate location recognition across contextual frequency thresholds ----
contextualAccuracy <- purrr::map_dfr(
  .x = 5:0,
  .f = \(threshold) {
    evaluateLocationRecognition(
      groundTruthCommentData = coGroundTruthLocations,
      comparisonCommentData = coWebCommentData |>
        dplyr::distinct(CommentID, Frequency, ContextualFrequency) |>
        dplyr::filter(Frequency >= 3, ContextualFrequency <= threshold)
    ) |> dplyr::mutate(`Contextual Threshold` = threshold, .before = 1)
  }
)

# save contextual accuracy table ----
saveRDS(object = contextualAccuracy, file = file.path(tablePath, contextualAccuracyTableName))

# location relationship graph accuracy ----

## isolate colorado ground truth comment relationships ----
coGroundTruthRelationships <- coGroundTruthCommentData |>
  purrr::map(
    .f = \(webComment) {
      webCommentRelationships <- webComment[["Relationships"]] |> dplyr::bind_rows()
      if (nrow(webCommentRelationships) > 0) {
        webCommentRelationships <- webCommentRelationships |>
          dplyr::mutate(CommentID = webComment[["CommentID"]], .before = "Location1")
      }
      return(webCommentRelationships)
    }
  ) |>
  purrr::list_rbind()

## evaluate location graph similarity ----
locationGraphSimilarity <- evaluateGraphSimilarity(
  commentIDs = unique(coWebCommentData[["CommentID"]]),
  groundTruthLocations = coGroundTruthLocations,
  groundTruthRelationships = coGroundTruthRelationships,
  comparisonRelationships = coWebCommentRelationships
)
