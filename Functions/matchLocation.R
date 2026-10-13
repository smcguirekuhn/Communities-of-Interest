
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
    locationMatches,
    mongeElkanThreshold = 0.15
  ) {
  
  # check arguments ----
  stopifnot(is.character(location))
  match.arg(adminLevel)
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts") %in% names(locationMatches)))
  
  # identify best location match by string distance ----
  if (adminLevel %in% c("Landmark", "School", "NA")) {
    locationMatch <- dplyr::tibble(Name = location, Match = NA)
  } else {
    locationMatch <- locationMatches |>
      dplyr::filter(AdminLevel == adminLevel) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        MongeElkanDistance = mongeElkanDistance(x = Name, y = location),
        CosineDistance = stringdist::stringdist(a = Name, b = location, method = "cosine"),
        .before = "Precincts"
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(MongeElkanDistance <= mongeElkanThreshold) |>
      dplyr::slice_min(order_by = CosineDistance, n = 1, with_ties = FALSE) |>
      dplyr::rename(Match = Name) |>
      dplyr::mutate(Name = location, .before = "Match")
  }
  
  # check for unusual match counts ----
  if (nrow(locationMatch) == 0) {
    cli::cli_warn(message = glue::glue("No matches found for {location}"))
    locationMatch <- dplyr::tibble(Name = location, Match = NA)
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