
evaluateLocationGraphSimilarity <- function(
    locationNodes,
    groundTruthRelationships,
    comparisonRelationships
) {
  
  # create ground truth graph ----
  groundTruthGraph <- createLocationGraphs(
    locationNodes = locationNodes,
    relationships = groundTruthRelationships
  )
  
  # create comparison graph ----
  comparisonGraph <- createLocationGraphs(
    locationNodes = locationNodes,
    relationships = comparisonRelationships
  )
  
  # compare grouped graph edges ----
  correctGroupedEdges <- igraph::ecount(
    graph = groundTruthGraph[["GroupedGraph"]] %s%
      comparisonGraph[["GroupedGraph"]]
  )
  
  # assign comparison and ground truth grouped edge counts ----
  comparisonGroupedEdges <- igraph::ecount(graph = comparisonGraph[["GroupedGraph"]])
  groundTruthGroupedEdges <- igraph::ecount(graph = groundTruthGraph[["GroupedGraph"]])
  
  # calculate grouped edge precision ----
  groupedEdgePrecision <- ifelse(
    test = comparisonGroupedEdges == 0,
    yes = as.numeric(correctGroupedEdges == 0),
    no = correctGroupedEdges/comparisonGroupedEdges
  )
  
  # calculate grouped edge recall ----
  groupedEdgeRecall <- ifelse(
    test = groundTruthGroupedEdges == 0,
    yes = as.numeric(correctGroupedEdges == 0),
    no = correctGroupedEdges/groundTruthGroupedEdges
  )
  
  # compare separated graph edges ----
  correctSeparatedEdges <- igraph::ecount(
    graph = groundTruthGraph[["SeparatedGraph"]] %s%
      comparisonGraph[["SeparatedGraph"]]
  )
  
  # assign comparison and ground truth separated edge counts ----
  comparisonSeparatedEdges <- igraph::ecount(graph = comparisonGraph[["SeparatedGraph"]])
  groundTruthSeparatedEdges <- igraph::ecount(graph = groundTruthGraph[["SeparatedGraph"]])
  
  # calculate separated edge precision ----
  separatedEdgePrecision <- ifelse(
    test = comparisonSeparatedEdges == 0,
    yes = as.numeric(correctSeparatedEdges == 0),
    no = correctSeparatedEdges/comparisonSeparatedEdges
  )
  
  # calculate separated edge recall ----
  separatedEdgeRecall <- ifelse(
    test = groundTruthSeparatedEdges == 0,
    yes = as.numeric(correctSeparatedEdges == 0),
    no = correctSeparatedEdges/groundTruthSeparatedEdges
  )
  
  # compile performance metrics data frame ----
  performanceMetrics <- dplyr::tibble(
    `Grouped Edge Precision` = round(groupedEdgePrecision, digits = 3),
    `Grouped Edge Recall` = round(groupedEdgeRecall, digits = 3),
    `Grouped Graph Accuracy` = groupedEdgePrecision == 1 & groupedEdgeRecall == 1,
    `Separated Edge Precision` = round(separatedEdgePrecision, digits = 3),
    `Separated Edge Recall` = round(separatedEdgeRecall, digits = 3),
    `Separated Graph Accuracy` = separatedEdgePrecision == 1 & separatedEdgeRecall == 1,
  )
  
  # return performance metrics data frame ----
  return(performanceMetrics)
}