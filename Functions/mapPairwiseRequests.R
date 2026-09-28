
mapPairwiseRequests <- function(state, commentID, locationNodes, requests) {
  
  # check arguments ----
  stopifnot(is.character(state))
  stopifnot(is.numeric(commentID))
  stopifnot(all(is.character(locationNodes)))
  stopifnot(is.data.frame(requests))
  
  # create nested sets of grouped and separated locations ----
  if (nrow(requests) > 0) {
    
    ## create location graphs ----
    locationGraphs <- createLocationGraphs(
      locationNodes = locationNodes,
      requests = requests
    )
    
    ## format location requests ----
    locationRequests <- locationGraphs |>
      purrr::pluck("GroupedGraph") |>
      igraph::components() |>
      purrr::pluck("membership") |>
      tibble::enframe(name = "Location", value = "Membership") |>
      dplyr::mutate(
        Separations = purrr::map(
          .x = Location,
          .f = \(location) {
            locationGraphs |>
              purrr::pluck("SeparatedGraph") |>
              igraph::neighbors(v = location) |>
              names() |>
              tibble::as_tibble_col(column_name = "Location")
          }
        )
      ) |>
      tidyr::nest(Groupings = Location) |>
      dplyr::select(-Membership) |>
      dplyr::relocate(Separations, .after = Groupings) |>
      dplyr::mutate(State = state, CommentID = commentID, .before = 1)
  } else {
    locationRequests <- dplyr::tibble(
      State = state,
      CommentID = commentID,
      Location = locationNodes,
      Separations = list(dplyr::tibble(Location = character(0)))
    ) |>
      tidyr::nest(Groupings = Location) |>
      dplyr::relocate(Separations, .after = Groupings)
  }
  
  # return location requests ----
  return(locationRequests)
}