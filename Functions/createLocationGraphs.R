
createLocationGraphs <- function(locationNodes, relationships) {
  
  # create grouped locations graph ----
  groupedGraph <- createGraphFromRelationships(
    locationNodes = locationNodes,
    relationships = relationships,
    type = "grouped"
  )
  
  # evaluate grouped graph components ----
  groupedGraphComponents <- igraph::components(groupedGraph)
  
  # find additional edges to add to grouped graph ---
  groupedEdgesToAdd <- purrr::map(
    .x = 1:groupedGraphComponents[["no"]],
    .f = \(component) {
      nodeIDs <- groupedGraphComponents[["membership"]] |>
        purrr::keep(.p = \(membershipID) membershipID == component)
      if (length(nodeIDs) > 1) {
        componentEdgesToAdd <- nodeIDs |> names() |> combn(m = 2) |> t()
      } else {
        componentEdgesToAdd <- NULL
      }
      return(componentEdgesToAdd)
    }
  ) |> purrr::list_c()
  
  # make grouped graph fully dense within components ----
  if (length(groupedEdgesToAdd) > 0) {
    groupedGraph <- groupedGraph + igraph::edges(t(groupedEdgesToAdd))
  }
  
  # simplify grouped graph ----
  groupedGraph <- groupedGraph |> igraph::simplify()
  
  # create grouped locations graph ----
  separatedGraph <- createGraphFromRelationships(
    locationNodes = locationNodes,
    relationships = relationships,
    type = "separated"
  )
  
  # find additional edges to add to grouped graph ---
  separatedEdgesToAdd <- purrr::map(
    .x = locationNodes,
    .f = \(locationNode) {
      separatedNodes <- separatedGraph |> igraph::neighbors(v = locationNode)
      separatedComponents <- groupedGraphComponents[["membership"]][separatedNodes] |> unique()
      if (length(separatedComponents) > 0) {
        componentEdgesToAdd <- data.frame(
          LocationNode = locationNode,
          SeparatedNodes = groupedGraphComponents[["membership"]] |>
            purrr::keep(.p = \(membershipID) membershipID %in% separatedComponents) |>
            names()
        )
      } else {
        componentEdgesToAdd <- NULL
      }
      return(componentEdgesToAdd)
    }
  ) |> purrr::list_rbind()
  
  # make separated graph fully dense across components ----
  if (nrow(separatedEdgesToAdd) > 0) {
    separatedEdgesToAdd <- separatedEdgesToAdd |> as.matrix(ncol = 2) |> t()
    separatedGraph <- separatedGraph + igraph::edges(separatedEdgesToAdd)
  }
  
  # simplify separated graph ----
  separatedGraph <- separatedGraph |> igraph::simplify()
  
  # compile graph list ----
  graphList <- list(
    GroupedGraph = groupedGraph,
    SeparatedGraph = separatedGraph
  )
  
  # return graph list ----
  return(graphList)
}

createGraphFromRelationships <- function(
    locationNodes,
    relationships,
    type = c("grouped", "separated")
) {
  
  # check arguments ----
  stopifnot(all(is.character(locationNodes)))
  stopifnot(is.data.frame(relationships))
  stopifnot(all(c("Relationship", "Location1", "Location2") %in% names(relationships)))
  match.arg(arg = type, several.ok = FALSE)
  
  # create locations graph ----
  locationsGraph <- igraph::make_empty_graph(directed = FALSE) + igraph::vertices(locationNodes)
  
  # format edges to add ----
  locationEdges <- relationships |>
    dplyr::filter(Relationship == type) |>
    dplyr::select(Location1, Location2) |>
    as.matrix(ncol = 2) |>
    t()
  
  # add edges if any are present ----
  if (length(locationEdges) > 0) locationsGraph <- locationsGraph + igraph::edges(locationEdges)
  
  # return locations graph ----
  return(locationsGraph)
}