
calculateGroupedCongruence <- function(
    statePrecinctMatrix,
    groupedPrecincts,
    simulationColumns = c("cd_2020", paste0("V", 2:5001)),
    districtCount = 8
  ) {
  
  # check arguments ----
  stopifnot(is.data.frame(statePrecinctMatrix))
  stopifnot(is.data.frame(groupedPrecincts))
  stopifnot("PrecinctID" %in% names(statePrecinctMatrix))
  stopifnot(all(c("PrecinctID", "Population") %in% names(groupedPrecincts)))
  stopifnot(is.numeric(districtCount))
  
  # isolate grouped precinct matrix ----
  groupedPrecinctMatrix <- statePrecinctMatrix |>
    dplyr::select(PrecinctID, dplyr::all_of(simulationColumns)) |>
    dplyr::inner_join(
      groupedPrecincts |> dplyr::select(PrecinctID, Population, Weight),
      by = "PrecinctID"
    )
  
  # calculate combined weights ----
  weights <- groupedPrecinctMatrix$Population*groupedPrecinctMatrix$Weight
  
  # calculate congruence for each simulation ----
  congruenceValues <- purrr::map_dbl(
    .x = simulationColumns,
    .f = \(simulation) {
      districtTotals <- rowsum(
        x = weights,
        group = factor(groupedPrecinctMatrix[[simulation]], levels = 1:districtCount),
        reorder = FALSE
      )
      proportions <- districtTotals/sum(districtTotals)
      return(sum(proportions^2))
    }
  )
  
  # return congruence values ----
  return(dplyr::tibble(Simulation = simulationColumns, Congruence = congruenceValues))
}