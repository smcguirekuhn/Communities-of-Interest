
evaluateABTestRecall <- function(
    groundTruthLocations,
    comparisonLocationsA,
    comparisonLocationsB
) {
  
  # check arguments ----
  stopifnot(is.data.frame(groundTruthLocations))
  stopifnot(is.data.frame(comparisonLocationsA))
  stopifnot(is.data.frame(comparisonLocationsB))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(groundTruthLocations)))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(comparisonLocationsA)))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(comparisonLocationsB)))
  
  # join ground truth and comparison data frames for recall test ----
  matchedLocations <- groundTruthLocations |>
    joyn::left_join(y = comparisonLocationsA, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
    dplyr::mutate(MatchedA = .joyn == "x & y") |>
    dplyr::select(-.joyn) |>
    joyn::left_join(y = comparisonLocationsB, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
    dplyr::mutate(MatchedB = .joyn == "x & y") |>
    dplyr::select(-.joyn) |>
    dplyr::mutate(Contrast = MatchedB - MatchedA)
  
  # create recall model with robust standard errors ----
  recallModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedLocations)
  
  # create table of recall statistics ----
  recallTable <- dplyr::tibble(
    `Location Recall A` = matchedLocations |> dplyr::pull(MatchedA) |> mean(),
    `Location Recall B` = matchedLocations |> dplyr::pull(MatchedB) |> mean(),
    `Contrast Regression Intercept` = recallModel$coefficients["(Intercept)"],
    `Contrast Regression P-Value` = recallModel$p.value["(Intercept)"]
  )
  
  # return table of recall statistics ----
  return(recallTable)
}