evaluateLocationRecognition <- function(groundTruthCommentData, comparisonCommentData) {
  
  # assign set of ground truth comment ids ----
  groundTruthCommentIDs <- unique(groundTruthCommentData[["CommentID"]])
  
  # match locations of both datasets based on monge elkan distance ----
  matchedLocations <- purrr::map_dfr(
    .x = groundTruthCommentIDs,
    .f = \(commentID) {
      
      ## filter datasets to an individual comment id ----
      commentLocationsGroundTruth <- groundTruthCommentData |> dplyr::filter(CommentID == commentID)
      commentLocationsComparison <- comparisonCommentData |> dplyr::filter(CommentID == commentID)
      
      ## cross join location names for both datasets ----
      crossJoinedLocations <- dplyr::cross_join(
        x = commentLocationsGroundTruth, 
        y = commentLocationsComparison
      )
      
      ## add monge elkan distances ----
      crossJoinedLocations <- crossJoinedLocations |>
        dplyr::rowwise() |>
        dplyr::mutate(
          MongeElkanDistance = mongeElkanDistance(
            x = `FullLocationName.x`,
            y = `FullLocationName.y`
          )
        ) |>
        dplyr::ungroup()
      
      ## filter to monge elkan distance threshold ----
      commentMatchedLocations <- crossJoinedLocations |>
        dplyr::filter(MongeElkanDistance == 0)
      
      ## add comment-wise precision and recall columns ----
      commentMatchedLocations <- commentMatchedLocations |>
        dplyr::mutate(
          CommentPrecision = nrow(commentLocationsComparison) == dplyr::n(),
          CommentRecall = nrow(commentLocationsGroundTruth) == dplyr::n()
        )
      
      ## return matched locations ----
      return(commentMatchedLocations)
    }
  )
  
  # select appropriate columns of matched locations ----
  matchedLocations <- matchedLocations |>
    dplyr::select(
      `CommentID.x`,
      `CommentID.y`,
      `FullLocationName.x`,
      `FullLocationName.y`,
      CommentPrecision,
      CommentRecall
    )
  
  # calculate performance metrics ----
  precision <- nrow(matchedLocations)/nrow(comparisonCommentData)
  commentPrecision <- matchedLocations |>
    dplyr::group_by(`CommentID.x`) |>
    dplyr::summarise(CommentPrecision = unique(CommentPrecision)) |>
    dplyr::filter(CommentPrecision == 1)
  recall <- nrow(matchedLocations)/nrow(groundTruthCommentData)
  commentRecall <- matchedLocations |>
    dplyr::group_by(`CommentID.x`) |>
    dplyr::summarise(CommentRecall = unique(CommentRecall)) |>
    dplyr::filter(CommentRecall == 1)
  f1Score <- 2*(precision*recall)/(precision + recall)
  
  # compile performance metrics data frame ----
  performanceMetrics <- dplyr::tibble(
    Precision = round(precision, digits = 3),
    `Comment Precision` = nrow(commentPrecision),
    Recall = round(recall, digits = 3),
    `Comment Recall` = nrow(commentRecall),
    `F1 Score` = round(f1Score, digits = 3)
  )
  
  # return performance metrics data frame ----
  return(performanceMetrics)
}