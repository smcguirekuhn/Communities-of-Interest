
evaluateABTestPrecision <- function(
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
      `Location Precision A` = matchedLocations |> dplyr::filter(Method == "A") |> dplyr::pull(Matched) |> mean(),
      `Location Precision B` = matchedLocations |> dplyr::filter(Method == "B") |> dplyr::pull(Matched) |> mean(),
      `Method Regression Coefficient` = precisionModel$coefficients["MethodB"],
      `Method Regression P-Value` = precisionModel$p.value["MethodB"]
    )
  } else {
    
    ## join ground truth and comparison data frames for precision test ----
    matchedCommentsA <- comparisonLocationsA |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "A", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::group_by(State, CommentID) |>
      dplyr::summarise(PrecisionA = sum(Matched)/dplyr::n())
    matchedCommentsB <- comparisonLocationsB |>
      joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
      dplyr::mutate(Method = "B", Matched = .joyn == "x & y") |>
      dplyr::select(-.joyn) |>
      dplyr::group_by(State, CommentID) |>
      dplyr::summarise(PrecisionB = sum(Matched)/dplyr::n())
    matchedComments <- dplyr::full_join(
      x = matchedCommentsA,
      y = matchedCommentsB,
      by = c("State", "CommentID")
    ) |> dplyr::mutate(Contrast = PrecisionB - PrecisionA)
    
    ## create precision model with robust standard errors ----
    precisionModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedComments)
    
    ## create table of precision statistics ----
    precisionTable <- dplyr::tibble(
      `Comment Precision A` = matchedComments |> dplyr::pull(PrecisionA) |> mean(),
      `Comment Precision B` = matchedComments |> dplyr::pull(PrecisionB) |> mean(),
      `Method Regression Coefficient` = precisionModel$coefficients["(Intercept)"],
      `Method Regression P-Value` = precisionModel$p.value["(Intercept)"]
    )
  }
  
  # return table of precision statistics ----
  return(precisionTable)
}