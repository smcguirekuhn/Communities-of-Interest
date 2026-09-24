
evaluateABLocationAccuracy <- function(
    groundTruthLocations,
    comparisonLocationsA,
    comparisonLocationsB,
    unit = c("location", "comment")
  ) {
  
  # conduct full suite of regression tests for recall and precision ----
  accuracyTable <- dplyr::bind_rows(
    evaluateABLocationRecall(
      groundTruthLocations = groundTruthLocations,
      comparisonLocationsA = comparisonLocationsA,
      comparisonLocationsB = comparisonLocationsB,
      unit = "location"
    ),
    evaluateABLocationRecall(
      groundTruthLocations = groundTruthLocations,
      comparisonLocationsA = comparisonLocationsA,
      comparisonLocationsB = comparisonLocationsB,
      unit = "comment"
    ),
    evaluateABLocationPrecision(
      groundTruthLocations = groundTruthLocations,
      comparisonLocationsA = comparisonLocationsA,
      comparisonLocationsB = comparisonLocationsB,
      unit = "location"
    ),
    evaluateABLocationPrecision(
      groundTruthLocations = groundTruthLocations,
      comparisonLocationsA = comparisonLocationsA,
      comparisonLocationsB = comparisonLocationsB,
      unit = "comment"
    )
  )
  
  # return accuracy table ----
  return(accuracyTable)
}

evaluateABLocationRecall <- function(
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

evaluateABLocationPrecision <- function(
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
  
  # calculate precision model statistics based on unit of aggregation ----
  if (unit == "location") {
    
    ## join ground truth and comparison data frames for precision test ----
    matchedLocationsA <- comparisonLocationsA |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "A", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn)
    matchedLocationsB <- comparisonLocationsB |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "B", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn)
    matchedLocations <- dplyr::bind_rows(matchedLocationsA, matchedLocationsB)
    
    ## create precision model with robust standard errors ----
    precisionModel <- estimatr::lm_robust(formula = Matched ~ State + Method, data = matchedLocations)
    
    ## create table of precision statistics ----
    precisionTable <- dplyr::tibble(
      Test = "Location Precision",
      Unit = "Location",
      `Method A` = matchedLocations |> dplyr::filter(Method == "A") |> dplyr::pull(Matched) |> mean(),
      `Method B` = matchedLocations |> dplyr::filter(Method == "B") |> dplyr::pull(Matched) |> mean(),
      `Coefficient` = precisionModel$coefficients["MethodB"],
      `P-Value` = precisionModel$p.value["MethodB"]
    )
  } else {
    
    ## build comment level recall data frames ----
    matchedLocationsA <- comparisonLocationsA |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "A", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::group_by(State, CommentID, Method) |>
      dplyr::summarise(Precision = sum(Matched)/dplyr::n())
    matchedLocationsB <- comparisonLocationsB |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "B", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::group_by(State, CommentID, Method) |>
      dplyr::summarise(Precision = sum(Matched)/dplyr::n())
    matchedLocations <- dplyr::bind_rows(matchedLocationsA, matchedLocationsB)
    
    ## create precision model with robust standard errors ----
    precisionModel <- estimatr::lm_robust(formula = Precision ~ State + Method, data = matchedLocations)
    
    ## create table of precision statistics ----
    precisionTable <- dplyr::tibble(
      Test = "Location Precision",
      Unit = "Comment",
      `Method A` = matchedLocations |> dplyr::filter(Method == "A") |> dplyr::pull(Precision) |> mean(),
      `Method B` = matchedLocations |> dplyr::filter(Method == "B") |> dplyr::pull(Precision) |> mean(),
      `Coefficient` = precisionModel$coefficients["MethodB"],
      `P-Value` = precisionModel$p.value["MethodB"]
    )
  }
  
  # return table of precision statistics ----
  return(precisionTable)
}
