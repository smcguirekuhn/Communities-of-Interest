
evaluateGraphSimilarity <- function(
    commentIDs,
    groundTruthLocations,
    groundTruthRelationships,
    comparisonRelationships
) {
  
  # check arguments ----
  stopifnot(is.data.frame(groundTruthLocations))
  stopifnot("CommentID" %in% names(groundTruthLocations))
  stopifnot(is.data.frame(groundTruthRelationships))
  stopifnot("CommentID" %in% names(groundTruthRelationships))
  stopifnot(is.data.frame(comparisonRelationships))
  stopifnot("CommentID" %in% names(comparisonRelationships))
  
  # evaluate location graph similarity for each comment ----
  locationGraphSimilarity <- purrr::map(
    .x = commentIDs,
    .f = \(commentID) {
      
      ## assign location nodes ----
      locationNodes <- groundTruthLocations |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::pull(FullLocationName)
      
      ## calculate comment similarity ----
      if (length(locationNodes) > 1) {
        commentSimilarity <- evaluateCommentGraphSimilarity(
          locationNodes = locationNodes,
          groundTruthRelationships = groundTruthRelationships |> dplyr::filter(CommentID == commentID),
          comparisonRelationships = comparisonRelationships |> dplyr::filter(CommentID == commentID)
        )
      } else {
        commentSimilarity <- NULL
      }
      
      ## return comment similarity ----
      return(commentSimilarity)
    }
  ) |>
    purrr::set_names(nm = unique(gaWebCommentData[["CommentID"]])) |>
    purrr::list_rbind(names_to = "CommentID")
  
  # compile graph similarity summary ----
  graphSimilaritySummary <- dplyr::tibble(
    
    ## total comments ----
    `Total Comments` = length(unique(locationGraphSimilarity[["CommentID"]])),
    
    ## grouped graph similarity ----
    `Mean Grouped Edge Precision` = locationGraphSimilarity[["Grouped Edge Precision"]] |>
      mean() |>
      round(digits = 3),
    `Mean Grouped Edge Recall` = locationGraphSimilarity[["Grouped Edge Recall"]] |>
      mean() |>
      round(digits = 3),
    `Grouped Comment Accuracy` = sum(locationGraphSimilarity[["Grouped Graph Accuracy"]]),
    
    ## separated graph similarity ----
    `Mean Separated Edge Precision` = locationGraphSimilarity[["Separated Edge Precision"]] |>
      mean() |>
      round(digits = 3),
    `Mean Separated Edge Recall` = locationGraphSimilarity[["Separated Edge Recall"]] |>
      mean() |>
      round(digits = 3),
    `Separated Comment Accuracy` = sum(locationGraphSimilarity[["Separated Graph Accuracy"]]),
    
    ## full comment accuracy ----
    `Full Comment Accuracy` = sum(locationGraphSimilarity[["Full Comment Accuracy"]])
  )
  
  # return graph similarity summary ----
  return(graphSimilaritySummary)
}

evaluateCommentGraphSimilarity <- function(
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
  
  # compile comment graph similarity data frame ----
  commentGraphSimilarity <- dplyr::tibble(
    `Grouped Edge Precision` = groupedEdgePrecision,
    `Grouped Edge Recall` = groupedEdgeRecall,
    `Grouped Graph Accuracy` = groupedEdgePrecision == 1 & groupedEdgeRecall == 1,
    `Separated Edge Precision` = separatedEdgePrecision,
    `Separated Edge Recall` = separatedEdgeRecall,
    `Separated Graph Accuracy` = separatedEdgePrecision == 1 & separatedEdgeRecall == 1,
  )
  
  # add full comment accuracy metric ----
  commentGraphSimilarity <- commentGraphSimilarity |>
    dplyr::mutate(`Full Comment Accuracy` = `Grouped Graph Accuracy` & `Separated Graph Accuracy`)
  
  # return comment graph similarity data frame ----
  return(commentGraphSimilarity)
}
