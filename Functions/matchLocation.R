
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
      "State House District",
      "State Senate District",
      "Congressional District",
      "Region",
      "NA"
    ),
    locationMatches
  ) {
  
  # check arguments ----
  stopifnot(is.character(location))
  match.arg(adminLevel)
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts", "Counties") %in% names(locationMatches)))
  
  # simplify municipal admin level assignments ----
  if (adminLevel %in% c("Township", "Borough", "Town", "City")) adminLevel <- "Municipality"
  
  # identify best location match by string distance ----
  locationMatch <- locationMatches |>
    dplyr::filter(AdminLevel == adminLevel) |>
    dplyr::rowwise() |>
    dplyr::mutate(MongeElkanDistance = mongeElkanDistance(x = Name, y = location), .before = "Precincts") |>
    dplyr::ungroup() |>
    dplyr::slice_min(order_by = MongeElkanDistance, n = 1) |>
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

# monge-elkan hybrid distance function
mongeElkanDistance <- function(x, y) {
  
  ## tokenize input strings ----
  x <- x |> tolower() 
  y <- y |> tolower()
  xTokens <- x |> regmatches(m = gregexpr("[a-z0-9]+", x)) |> unlist()
  yTokens <- y |> regmatches(m = gregexpr("[a-z0-9]+", y)) |> unlist()
  
  ## penalize empty strings ----
  if (length(xTokens) == 0 || length(yTokens) == 0) return(1.0)
  
  ## extract numeric elements each token set ----
  numberTokensX <- xTokens[grepl(pattern = "\\b\\d+\\b", x = xTokens)]
  numberTokensY <- yTokens[grepl(pattern = "\\b\\d+\\b", x = yTokens)]
  
  ## penalize distance for mismatched numbers ----
  if (length(numberTokensX) > 0 && length(numberTokensY) > 0) {
    if (!any(numberTokensX %in% numberTokensY)) return(0.9)
  }
  
  ## calculate jaro-winkler distance matrix ----
  distanceMatrix <- stringdist::stringdistmatrix(a = xTokens, b = yTokens, method = "jw", p = 0.1)
  
  ## aggregate across jaro-winkler distance matrix using monge-elkan method ----
  distance <- distanceMatrix |> apply(MARGIN = 1, FUN = min) |> mean()
  
  ## return monge-eklan distance ----
  return(distance)
}