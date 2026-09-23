
evaluateABTestRecall <- function(
    groundTruthLocations,
    comparisonLocationsA,
    comparisonLocationsB,
    unit = c("location", "comment")
) {
  
  # check arguments ----
  stopifnot(is.data.frame(groundTruthLocations))
  stopifnot(is.data.frame(comparisonLocationsA))
  stopifnot(is.data.frame(comparisonLocationsB))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(groundTruthLocations)))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(comparisonLocationsA)))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(comparisonLocationsB)))
  unit <- match.arg(arg = unit)
  
  # calculate recall model statistics based on unit of aggregation ----
  if (unit == "location") {
    
    ## join ground truth and comparison data frames for recall test ----
    matchedLocations <- groundTruthLocations |>
      joyn::left_join(y = comparisonLocationsA, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(MatchedA = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      joyn::left_join(y = comparisonLocationsB, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(MatchedB = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::mutate(Contrast = MatchedB - MatchedA)
    
    ## create recall model with robust standard errors ----
    recallModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedLocations)
    
    ## create table of recall statistics ----
    recallTable <- dplyr::tibble(
      Test = "Location Recall",
      Unit = "Location",
      `Method A` = matchedLocations |> dplyr::pull(MatchedA) |> mean(),
      `Method B` = matchedLocations |> dplyr::pull(MatchedB) |> mean(),
      `Coefficient` = recallModel$coefficients["(Intercept)"],
      `P-Value` = recallModel$p.value["(Intercept)"]
    )
  } else {
    
    ## join ground truth and comparison data frames for recall test ----
    matchedLocations <- groundTruthLocations |>
      joyn::left_join(y = comparisonLocationsA, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(MatchedA = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      joyn::left_join(y = comparisonLocationsB, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(MatchedB = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::group_by(State, CommentID) |>
      dplyr::summarise(RecallA = sum(MatchedA)/dplyr::n(), RecallB = sum(MatchedB)/dplyr::n()) |>
      dplyr::mutate(Contrast = RecallB - RecallA)
    
    ## create recall model with robust standard errors ----
    recallModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedLocations)
    
    ## create table of recall statistics ----
    recallTable <- dplyr::tibble(
      Test = "Location Recall",
      Unit = "Comment",
      `Method A` = matchedLocations |> dplyr::pull(RecallA) |> mean(),
      `Method B` = matchedLocations |> dplyr::pull(RecallB) |> mean(),
      `Coefficient` = recallModel$coefficients["(Intercept)"],
      `P-Value` = recallModel$p.value["(Intercept)"]
    )
  }
  
  # return table of recall statistics ----
  return(recallTable)
}