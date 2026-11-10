
calculateSeparatedCongruence <- function(
    statePrecinctMatrix,
    group1Precincts,
    group2Precincts,
    simulationColumns = c("cd_2020", paste0("V", 2:5001)),
    districtCount = 8
  ) {
  
  # check arguments ----
  stopifnot(is.data.frame(statePrecinctMatrix))
  stopifnot(is.data.frame(group1Precincts))
  stopifnot(is.data.frame(group2Precincts))
  stopifnot("PrecinctID" %in% names(statePrecinctMatrix))
  stopifnot(all(c("PrecinctID", "Population", "Weight") %in% names(group1Precincts)))
  stopifnot(all(c("PrecinctID", "Population", "Weight") %in% names(group2Precincts)))
  stopifnot(is.numeric(districtCount))
  
  # isolate group 1 precinct matrix ----
  group1PrecinctMatrix <- statePrecinctMatrix |>
    dplyr::select(PrecinctID, dplyr::all_of(simulationColumns)) |>
    dplyr::inner_join(
      group1Precincts |> dplyr::select(PrecinctID, Population, Weight),
      by = "PrecinctID"
    )
  
  # isolate group 2 precinct matrix ----
  group2PrecinctMatrix <- statePrecinctMatrix |>
    dplyr::select(PrecinctID, dplyr::all_of(simulationColumns)) |>
    dplyr::inner_join(
      group2Precincts |> dplyr::select(PrecinctID, Population, Weight),
      by = "PrecinctID"
    )
  
  # calculate combined weights ----
  weights1 <- group1PrecinctMatrix$Population*group1PrecinctMatrix$Weight
  weights2 <- group2PrecinctMatrix$Population*group2PrecinctMatrix$Weight
  
  # calculate congruence for each simulation ----
  congruenceValues <- purrr::map_dbl(
    .x = simulationColumns,
    .f = \(simulation) {
      
      ## calculate proportion in each district for group 1 ----
      districtTotals1 <- tapply(
        X = weights1,
        INDEX = factor(group1PrecinctMatrix[[simulation]], levels = 1:districtCount),
        FUN = sum,
        na.rm = TRUE
      )
      districtTotals1[is.na(districtTotals1)] <- 0
      proportions1 <- districtTotals1/sum(districtTotals1)
      
      ## calculate proportion in each district for group 2 ----
      districtTotals2 <- tapply(
        X = weights2,
        INDEX = factor(group2PrecinctMatrix[[simulation]], levels = 1:districtCount),
        FUN = sum,
        na.rm = TRUE
      )
      districtTotals2[is.na(districtTotals2)] <- 0
      proportions2 <- districtTotals2/sum(districtTotals2)
      
      ## return dot product of two proportions ----
      return(sum(proportions1*(1 - proportions2)))
    }
  )
  
  # return congruence values ----
  return(dplyr::tibble(Simulation = simulationColumns, Congruence = congruenceValues))
}