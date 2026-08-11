evaluateGroundTruthAccuracy <- function(groundTruthComments, comparisonComments) {
  
  # evaluate correctly identified location names ----
  correctLocations <- dplyr::inner_join(
    x = groundTruthComments,
    y = comparisonComments,
    by = c("CommentID", "Name", "AdminLevel"),
    relationship = "many-to-many"
  ) |> dplyr::distinct(CommentID, Name, AdminLevel, .keep_all = TRUE)
  
  # report correctly identified locations percentage ----
  cli::cli_inform(
    message = c(
      "i" = "Correctly identified locations:",
      "*" = glue::glue(
        "{nrow(correctLocations)} out of {nrow(groundTruthComments)}
        ({round(100*nrow(correctLocations)/nrow(groundTruthComments))}%)"
      )
    )
  )
  
  # evaluate correlation between sentiment scores ----
  sentimentCorrelation <- cor(
    x = correctLocations |> dplyr::distinct(CommentID, .keep_all = TRUE) |> dplyr::pull(`Sentiment.x`),
    y = correctLocations |> dplyr::distinct(CommentID, .keep_all = TRUE) |> dplyr::pull(`Sentiment.y`)
  )
  
  # report sentiment correlation ----
  cli::cli_inform(
    message = c(
      "i" = "Sentiment scores correlation among correctly identified locations:",
      "*" = glue::glue("{round(sentimentCorrelation, digits = 2)}")
    )
  )
  
  # evaluate correlation between clarity scores ----
  clarityCorrelation <- cor(
    x = correctLocations |> dplyr::distinct(CommentID, .keep_all = TRUE) |> dplyr::pull(`Clarity.x`),
    y = correctLocations |> dplyr::distinct(CommentID, .keep_all = TRUE) |> dplyr::pull(`Clarity.y`)
  )
  
  # report clarity correlation ----
  cli::cli_inform(
    message = c(
      "i" = "Clarity scores correlation among correctly identified locations:",
      "*" = glue::glue("{round(clarityCorrelation, digits = 2)}")
    )
  )
  
  # add district type agreement variables ----
  correctLocations <- correctLocations |>
    dplyr::mutate(
      
      # evaluate number of correctly identified locations to group ----
      DistrictTypesSimilarity = purrr::pmap_int(
        .l = list(DistrictTypes.x, DistrictTypes.y),
        .f = \(groundTruthDistrictTypes, comparisonDistrictTypes) {
          if (length(groundTruthDistrictTypes) == 0) groundTruthDistrictTypes <- "NA"
          if (length(comparisonDistrictTypes) == 0) comparisonDistrictTypes <- "NA"
          districtTypesInCommon <- intersect(
            x = groundTruthDistrictTypes |> unlist(),
            y = comparisonDistrictTypes |> unlist()
          )
          return(length(districtTypesInCommon))
        }
      ),
      
      # evaluate length of ground truth district types ----
      DistrictTypesLength = purrr::pmap_int(
        .l = list(DistrictTypes.x),
        .f = \(groundTruthDistrictTypes) {
          if (length(groundTruthDistrictTypes) == 0) groundTruthDistrictTypes <- "NA"
          return(length(groundTruthDistrictTypes))
        }
      )
    )
  
  # compile district type accuracy metrics ----
  districtTypesAggregateAccuracy <- sum(correctLocations[["DistrictTypesSimilarity"]])/
    sum(correctLocations[["DistrictTypesLength"]])
  districtTypesMeanAccuracy <- mean(
    correctLocations[["DistrictTypesSimilarity"]]/
      correctLocations[["DistrictTypesLength"]]
  )
  
  # report locations to group accuracy ----
  cli::cli_inform(
    message = c(
      "i" = "District types accuracy among correctly identified locations:",
      "*" = glue::glue("Aggregate: {round(districtTypesAggregateAccuracy, digits = 2)}"),
      "*" = glue::glue("Location-Wise Mean: {round(districtTypesMeanAccuracy, digits = 2)}")
    )
  )
  
  # add location grouping agreement variables ----
  correctLocations <- correctLocations |>
    dplyr::mutate(
      
      # evaluate number of correctly identified locations to group ----
      LocationsToGroupSimilarity = purrr::pmap_int(
        .l = list(LocationsToGroup.x, LocationsToGroup.y),
        .f = \(groundTruthLocations, comparisonLocations) {
          if (length(groundTruthLocations) == 0) groundTruthLocations <- "NA"
          if (length(comparisonLocations) == 0) comparisonLocations <- "NA"
          locationsInCommon <- intersect(
            x = groundTruthLocations |> unlist(),
            y = comparisonLocations |> unlist()
          )
          return(length(locationsInCommon))
        }
      ),
      
      # evaluate length of ground truth locations to group ----
      LocationsToGroupLength = purrr::pmap_int(
        .l = list(LocationsToGroup.x),
        .f = \(groundTruthLocations) {
          if (length(groundTruthLocations) == 0) groundTruthLocations <- "NA"
          return(length(groundTruthLocations))
        }
      ),
      
      # evaluate number of correctly identified locations to separate ----
      LocationsToSeparateSimilarity = purrr::pmap_int(
        .l = list(LocationsToSeparate.x, LocationsToSeparate.y),
        .f = \(groundTruthLocations, comparisonLocations) {
          if (length(groundTruthLocations) == 0) groundTruthLocations <- "NA"
          if (length(comparisonLocations) == 0) comparisonLocations <- "NA"
          locationsInCommon <- intersect(
            x = groundTruthLocations |> unlist(),
            y = comparisonLocations |> unlist()
          )
          return(length(locationsInCommon))
        }
      ),
      
      # evaluate length of ground truth locations to separate ----
      LocationsToSeparateLength = purrr::pmap_int(
        .l = list(LocationsToSeparate.x),
        .f = \(groundTruthLocations) {
          if (length(groundTruthLocations) == 0) groundTruthLocations <- "NA"
          return(length(groundTruthLocations))
        }
      )
    )
  
  # compile location grouping accuracy metrics ----
  locationsToGroupAggregateAccuracy <- sum(correctLocations[["LocationsToGroupSimilarity"]])/
    sum(correctLocations[["LocationsToGroupLength"]])
  locationsToGroupMeanAccuracy <- mean(
    correctLocations[["LocationsToGroupSimilarity"]]/
      correctLocations[["LocationsToGroupLength"]]
  )
  
  # report locations to group accuracy ----
  cli::cli_inform(
    message = c(
      "i" = "Grouped locations accuracy among correctly identified locations:",
      "*" = glue::glue("Aggregate: {round(locationsToGroupAggregateAccuracy, digits = 2)}"),
      "*" = glue::glue("Location-Wise Mean: {round(locationsToGroupMeanAccuracy, digits = 2)}")
    )
  )
  
  # compile location separation accuracy metrics ----
  locationsToSeparateAggregateAccuracy <- sum(correctLocations[["LocationsToSeparateSimilarity"]])/
    sum(correctLocations[["LocationsToSeparateLength"]])
  locationsToSeparateMeanAccuracy <- mean(
    correctLocations[["LocationsToSeparateSimilarity"]]/
      correctLocations[["LocationsToSeparateLength"]]
  )
  
  # report locations to separate accuracy ----
  cli::cli_inform(
    message = c(
      "i" = "Separated locations accuracy among correctly identified locations:",
      "*" = glue::glue("Aggregate: {round(locationsToSeparateAggregateAccuracy, digits = 2)}"),
      "*" = glue::glue("Location-Wise Mean: {round(locationsToSeparateMeanAccuracy, digits = 2)}")
    )
  )
}