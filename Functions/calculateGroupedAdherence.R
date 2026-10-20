
calculateGroupedAdherence <- function(
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
    dplyr::inner_join(y = groupedPrecincts, by = "PrecinctID") |>
    dplyr::mutate(Population = Population*Weight) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(simulationColumns), \(x) factor(x, levels = 1:districtCount)))
  
  # calculate adherence values ----
  adherenceValues <- simulationColumns |>
    purrr::map(
      .f = \(simulation) {
        stats::xtabs(
          formula = paste0("Population ~ ", simulation),
          data = groupedPrecinctMatrix
        ) |> prop.table() |> (\(x) x^2)() |> sum()
      }
    ) |>
    purrr::list_c() |>
    stats::setNames(nm = simulationColumns) |>
    tibble::enframe(name = "Simulation", value = "Adherence")
  
  # return adherence values ----
  return(adherenceValues)
}