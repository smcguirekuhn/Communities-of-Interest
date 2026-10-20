
calculateSeparatedAdherence <- function(
    statePrecinctMatrix,
    groupedPrecincts,
    separatedPrecincts,
    simulationColumns = c("cd_2020", paste0("V", 2:5001)),
    districtCount = 8
  ) {
  
  # check arguments ----
  stopifnot(is.data.frame(statePrecinctMatrix))
  stopifnot(is.data.frame(groupedPrecincts))
  stopifnot("PrecinctID" %in% names(statePrecinctMatrix))
  stopifnot(all(c("PrecinctID", "Population", "Weight") %in% names(groupedPrecincts)))
  stopifnot(all(c("PrecinctID", "Population", "Weight") %in% names(separatedPrecincts)))
  stopifnot(is.numeric(districtCount))
  
  # isolate grouped precinct matrix ----
  groupedPrecinctMatrix <- statePrecinctMatrix |>
    dplyr::inner_join(y = groupedPrecincts, by = "PrecinctID") |>
    dplyr::mutate(Population = Population*Weight) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(simulationColumns), \(x) factor(x, levels = 1:districtCount)))
  
  # isolate separated precinct matrix ----
  separatedPrecinctMatrix <- statePrecinctMatrix |>
    dplyr::inner_join(y = separatedPrecincts, by = "PrecinctID") |>
    dplyr::mutate(Population = Population*Weight) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(simulationColumns), \(x) factor(x, levels = 1:districtCount)))
  
  # calculate adherence values ----
  adherenceValues <- simulationColumns |>
    purrr::map(
      .f = \(simulation) {
        groupedDistrictProportions <- stats::xtabs(
          formula = paste0("Population ~ ", simulation),
          data = groupedPrecinctMatrix
        ) |> prop.table()
        separatedDistrictProportions <- stats::xtabs(
          formula = paste0("Population ~ ", simulation),
          data = separatedPrecinctMatrix
        ) |> prop.table()
        return(sum(groupedDistrictProportions*(1 - separatedDistrictProportions)))
      }
    ) |>
    purrr::list_c() |>
    stats::setNames(nm = simulationColumns) |>
    tibble::enframe(name = "Simulation", value = "Adherence")
  
  # return adherence values ----
  return(adherenceValues)
}