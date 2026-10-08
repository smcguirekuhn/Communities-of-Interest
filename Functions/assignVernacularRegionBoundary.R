
assignVernacularRegionBoundary <- function(
    regionName,
    state,
    countyBoundaries,
    iterations = 10,
    threshold = 5,
    model = "mistral-large-2512"
) {
  
  # check arguments ----
  stopifnot(is.character(regionName))
  stopifnot(state %in% state.name)
  stopifnot(is.data.frame(countyBoundaries))
  stopifnot("Name" %in% names(countyBoundaries))
  stopifnot(is.numeric(iterations))
  stopifnot(is.numeric(threshold))
  stopifnot(threshold <= iterations)
  stopifnot(is.character(model))
  
  # assign counties within the vernacular region ----
  countyNames <- purrr::map_dfr(
    .x = 1:10,
    .f = \(iteration) {
      chat <- ellmer::chat_mistral(model = model)
      regionCounties <- chat$chat_structured(
        ellmer::interpolate(
          "Return the counties in {{state}} that are likely part of
          the following vernacular region: {{regionName}}",
          state = state,
          regionName = regionName
        ),
        type = ellmer::type_array(
          items = ellmer::type_enum(values = countyBoundaries[["Name"]])
        )
      )
      return(dplyr::tibble(Name = regionCounties))
    }
  )
  
  # summarize counts of named counties ----
  countyNames <- countyNames |> dplyr::group_by(Name) |> dplyr::summarise(Count = dplyr::n())
  
  # combine counties into a single boundary ----
  regionBoundary <- countyBoundaries |>
    dplyr::right_join(y = countyNames, by = "Name") |>
    dplyr::filter(Count > threshold) |>
    dplyr::mutate(Weight = (Count - threshold)/(iterations - threshold))
  
  # return region boundary ----
  return(regionBoundary)
}