
mapCOIPrecincts <- function(
    commentInfo,
    locationMatches,
    precinctBoundaries,
    titleFormat = glue::glue(
      "Comment {commentInfo$CommentID}: ",
      "{commentInfo$Name} of {commentInfo$County}"
    )
  ) {
  
  # check arguments ----
  stopifnot(length(titleFormat) == 1)
  stopifnot("character" %in% class(titleFormat))
  
  # match comment coi locations ----
  locationsMentioned <- matchCOILocations(
    commentInfo = commentInfo,
    locationMatches = locationMatches,
    precinctBoundaries = precinctBoundaries
  )
  
  # create coi map ----
  coiMap <- locationsMentioned |>
    dplyr::mutate(Group = factor(Group)) |>
    dplyr::group_by(Name) |>
    dplyr::summarise(
      Precincts = dplyr::n(),
      Group = unique(Group),
      Match = unique(Match)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = Group),
      alpha = 0.3,
      color = "#AAAAAA",
      linewidth = 0.5
    ) +
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(label = Match, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      box.padding = 0.5,
      max.overlaps = Inf,
      alpha = 0.3
    ) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(
      title = titleFormat,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()
  
  # return coi map ----
  return(coiMap)
}