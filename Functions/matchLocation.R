matchLocation <- function(
    location,
    adminLevel = c(
      "Landmark",
      "School",
      "Neighborhood",
      "Township",
      "Borough",
      "Town",
      "City",
      "School District",
      "County",
      "Legislative District",
      "Region",
      "NA"
    ),
    surroundingCounties,
    locationMatches
  ) {
  
  # check arguments ----
  stopifnot(is.character(location))
  match.arg(adminLevel)
  stopifnot(all(is.character(surroundingCounties)))
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts", "Counties") %in% names(locationMatches)))
  
  # simplify municipal admin level assignments ----
  if (adminLevel %in% c("Township", "Borough", "Town", "City")) adminLevel <- "Municipality"
  
  # remove "county" from the ends of surrounding county names ----
  surroundingCounties <- surroundingCounties |> stringr::str_remove(pattern = " County$")
  
  # identify best location match by string distance ----
  locationMatch <- locationMatches |>
    dplyr::filter(
      AdminLevel == adminLevel,
      purrr::map_lgl(.x = Counties, .f = \(counties) any(counties[["County"]] %in% surroundingCounties))
    ) |>
    dplyr::mutate(
      JaccardDistance = stringdist::stringdist(a = Name, b = location, method = "jaccard"),
      JaroWinklerDistance = stringdist::stringdist(a = Name, b = location, method = "jw"),
      .before = "Precincts"
    ) |>
    dplyr::slice_min(order_by = dplyr::tibble(JaccardDistance, JaroWinklerDistance)) |>
    dplyr::rename(Match = Name) |>
    dplyr::mutate(Name = location, .before = "Match")
  
  # check for unusual match counts ----
  if (nrow(locationMatch) == 0) {
    cli::cli_warn(message = glue::glue("No matches found for {location}"))
  } else if (nrow(locationMatch) > 1) {
    cli::cli_warn(message = glue::glue("Multiple matches found for {location}"))
  }
  
  # return location match ----
  return(locationMatch)
}