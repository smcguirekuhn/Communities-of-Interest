
evaluateABTestPrecision <- function(
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
  
  # join ground truth and comparison data frames for precision test ----
  matchedLocationsA <- comparisonLocationsA |>
    joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
    dplyr::mutate(Method = "A", Matched = .joyn == "x & y") |>
    dplyr::select(-.joyn)
  matchedLocationsB <- comparisonLocationsB |>
    joyn::left_join(y = allGroundTruthLocations, by = c("State", "CommentID", "FullLocationName"), keep = FALSE) |>
    dplyr::mutate(Method = "B", Matched = .joyn == "x & y") |>
    dplyr::select(-.joyn)
  matchedLocations <- dplyr::bind_rows(matchedLocationsA, matchedLocationsB)
  
  # create precision model with robust standard errors ----
  precisionModel <- estimatr::lm_robust(formula = Matched ~ State + Method, data = matchedLocations)
  
  # create table of precision statistics ----
  precisionTable <- dplyr::tibble(
    `Location Precision A` = matchedLocations |> dplyr::filter(Method == "A") |> dplyr::pull(Matched) |> mean(),
    `Location Precision B` = matchedLocations |> dplyr::filter(Method == "B") |> dplyr::pull(Matched) |> mean(),
    `Method Regression Coefficient` = precisionModel$coefficients["MethodB"],
    `Method Regression P-Value` = precisionModel$p.value["MethodB"]
  )
  
  # return table of precision statistics ----
  return(precisionTable)
}