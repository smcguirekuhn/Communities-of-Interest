
evaluateABRequestAccuracy <- function(
    groundTruthLocations,
    groundTruthRequests,
    comparisonRequestsA,
    comparisonRequestsB
  ) {
  
  # compile request level accuracy table ----
  accuracyData <- compileAccuracyData(
    groundTruthLocations = groundTruthLocations,
    groundTruthRequests = groundTruthRequests,
    comparisonRequestsA = comparisonRequestsA,
    comparisonRequestsB = comparisonRequestsB
  )
  
  # conduct full suite of regression tests for grouped recall and precision ----
  groupedAccuracyTable <- dplyr::bind_rows(
    evaluateABRequestRecall(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "grouped"),
      unit = "location pair"
    ),
    evaluateABRequestRecall(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "grouped"),
      unit = "comment"
    ),
    evaluateABRequestPrecision(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "grouped"),
      unit = "location pair"
    ),
    evaluateABRequestPrecision(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "grouped"),
      unit = "comment"
    )
  )
  
  # conduct full suite of regression tests for separated recall and precision ----
  separatedAccuracyTable <- dplyr::bind_rows(
    evaluateABRequestRecall(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "separated"),
      unit = "location pair"
    ),
    evaluateABRequestRecall(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "separated"),
      unit = "comment"
    ),
    evaluateABRequestPrecision(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "separated"),
      unit = "location pair"
    ),
    evaluateABRequestPrecision(
      accuracyData = accuracyData |> dplyr::filter(`Edge Type` == "separated"),
      unit = "comment"
    )
  )
  
  # compile full accuracy table ----
  accuracyTable <- dplyr::bind_rows(
    groupedAccuracyTable |> dplyr::mutate(Test = paste("Grouped", Test)),
    separatedAccuracyTable |> dplyr::mutate(Test = paste("Separated", Test)),
  )
  
  # return accuracy table ----
  return(accuracyTable)
}


evaluateABRequestRecall <- function(accuracyData, unit = c("location pair", "comment")) {
  
  # check arguments ----
  unit <- match.arg(arg = unit)
  
  # calculate recall model statistics based on unit of aggregation ----
  if (unit == "location pair") {
    
    ## build location-pair level recall contrast ----
    matchedRequests <- accuracyData |>
      dplyr::filter(`Ground Truth Edges`) |>
      dplyr::mutate(
        MatchedA = `Ground Truth Edges` & `Comparison Edges A`,
        MatchedB = `Ground Truth Edges` & `Comparison Edges B`,
        Contrast = MatchedB - MatchedA
      )
    
    ## create recall model with robust standard errors ----
    recallModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedRequests)
    
    ## create table of recall statistics ----
    recallTable <- dplyr::tibble(
      Test = "Request Recall",
      Unit = "Location Pair",
      `Method A` = matchedRequests |> dplyr::pull(MatchedA) |> mean(),
      `Method B` = matchedRequests |> dplyr::pull(MatchedB) |> mean(),
      `Intercept` = recallModel$coefficients["(Intercept)"],
      `P-Value` = recallModel$p.value["(Intercept)"]
    )
  } else {
    
    ## build location-pair level recall contrast ----
    matchedRequests <- accuracyData |>
      dplyr::filter(`Ground Truth Edges`) |>
      dplyr::mutate(
        MatchedA = `Ground Truth Edges` & `Comparison Edges A`,
        MatchedB = `Ground Truth Edges` & `Comparison Edges B`
      ) |>
      dplyr::group_by(State, CommentID) |>
      dplyr::summarise(RecallA = sum(MatchedA)/dplyr::n(), RecallB = sum(MatchedB)/dplyr::n()) |>
      dplyr::mutate(Contrast = RecallB - RecallA)
    
    ## create recall model with robust standard errors ----
    recallModel <- estimatr::lm_robust(formula = Contrast ~ State, data = matchedRequests)
    
    ## create table of recall statistics ----
    recallTable <- dplyr::tibble(
      Test = "Request Recall",
      Unit = "Comment",
      `Method A` = matchedRequests |> dplyr::pull(RecallA) |> mean(),
      `Method B` = matchedRequests |> dplyr::pull(RecallB) |> mean(),
      `Intercept` = recallModel$coefficients["(Intercept)"],
      `P-Value` = recallModel$p.value["(Intercept)"]
    )
  }
  
  # return recall table ----
  return(recallTable)
}


evaluateABRequestPrecision <- function(accuracyData, unit = c("location pair", "comment")) {
  
  # check arguments ----
  unit <- match.arg(arg = unit)
  
  # calculate recall model statistics based on unit of aggregation ----
  if (unit == "location pair") {
    
    ## build location-pair level precision data frames ----
    matchedRequestsA <- accuracyData |>
      dplyr::filter(`Comparison Edges A`) |>
      dplyr::mutate(Method = "A", Matched = `Ground Truth Edges` & `Comparison Edges A`) |>
      dplyr::select(State, CommentID, Method, Matched)
    matchedRequestsB <- accuracyData |>
      dplyr::filter(`Comparison Edges B`) |>
      dplyr::mutate(Method = "B", Matched = `Ground Truth Edges` & `Comparison Edges B`) |>
      dplyr::select(State, CommentID, Method, Matched)
    matchedRequests <- dplyr::bind_rows(matchedRequestsA, matchedRequestsB)
    
    ## create precision model with robust standard errors ----
    precisionModel <- estimatr::lm_robust(formula = Matched ~ State + Method, data = matchedRequests)
    
    ## create table of precision statistics ----
    precisionTable <- dplyr::tibble(
      Test = "Request Precision",
      Unit = "Location Pair",
      `Method A` = matchedRequests |> dplyr::filter(Method == "A") |> dplyr::pull(Matched) |> mean(),
      `Method B` = matchedRequests |> dplyr::filter(Method == "B") |> dplyr::pull(Matched) |> mean(),
      `Coefficient` = precisionModel$coefficients["MethodB"],
      `P-Value` = precisionModel$p.value["MethodB"]
    )
  } else {
    
    ## build comment level precision data frames ----
    matchedRequestsA <- accuracyData |>
      dplyr::filter(`Comparison Edges A`) |>
      dplyr::mutate(Method = "A", Matched = `Ground Truth Edges` & `Comparison Edges A`) |>
      dplyr::select(State, CommentID, Method, Matched) |>
      dplyr::group_by(State, CommentID, Method) |>
      dplyr::summarise(Precision = sum(Matched)/dplyr::n())
    matchedRequestsB <- accuracyData |>
      dplyr::filter(`Comparison Edges B`) |>
      dplyr::mutate(Method = "B", Matched = `Ground Truth Edges` & `Comparison Edges B`) |>
      dplyr::select(State, CommentID, Method, Matched) |>
      dplyr::group_by(State, CommentID, Method) |>
      dplyr::summarise(Precision = sum(Matched)/dplyr::n())
    matchedRequests <- dplyr::bind_rows(matchedRequestsA, matchedRequestsB)
    
    ## create precision model with robust standard errors ----
    precisionModel <- estimatr::lm_robust(formula = Precision ~ State + Method, data = matchedRequests)
    
    ## create table of precision statistics ----
    precisionTable <- dplyr::tibble(
      Test = "Request Precision",
      Unit = "Comment",
      `Method A` = matchedRequests |> dplyr::filter(Method == "A") |> dplyr::pull(Precision) |> mean(),
      `Method B` = matchedRequests |> dplyr::filter(Method == "B") |> dplyr::pull(Precision) |> mean(),
      `Coefficient` = precisionModel$coefficients["MethodB"],
      `P-Value` = precisionModel$p.value["MethodB"]
    )
  }
  
  # return precision table ----
  return(precisionTable)
}


compileAccuracyData <- function(
    groundTruthLocations,
    groundTruthRequests,
    comparisonRequestsA,
    comparisonRequestsB
  ) {
  
  # check arguments ----
  stopifnot(is.data.frame(groundTruthLocations))
  stopifnot(is.data.frame(groundTruthRequests))
  stopifnot(is.data.frame(comparisonRequestsA))
  stopifnot(is.data.frame(comparisonRequestsB))
  stopifnot(all(c("State", "CommentID", "FullLocationName") %in% names(groundTruthLocations)))
  stopifnot(all(c("State", "CommentID") %in% names(groundTruthRequests)))
  stopifnot(all(c("State", "CommentID") %in% names(comparisonRequestsA)))
  stopifnot(all(c("State", "CommentID") %in% names(comparisonRequestsB)))
  
  # compile request-level matrix accuracy data across all states and comments ----
  accuracyData <- purrr::map2_dfr(
    .x = groundTruthRequests |> dplyr::distinct(State, CommentID) |> dplyr::pull(State),
    .y = groundTruthRequests |> dplyr::distinct(State, CommentID) |> dplyr::pull(CommentID),
    .f = \(state, commentID) {
      
      ## set ground truth location nodes ----
      locationNodes <- groundTruthLocations |>
        dplyr::filter(State == state, CommentID == commentID) |>
        dplyr::pull(FullLocationName)
      
      ## create ground truth requests matrix ----
      groundTruthRequestsMatrix <- createRequestsMatrix(
        locationNodes = locationNodes,
        commentRequests = groundTruthRequests |> dplyr::filter(State == state, CommentID == commentID)
      )
      
      ## create comparison requests matrix a ----
      comparisonRequestsMatrixA <- createRequestsMatrix(
        locationNodes = locationNodes,
        commentRequests = comparisonRequestsA |> dplyr::filter(State == state, CommentID == commentID)
      )
      
      ## create comparison requests matrix b ----
      comparisonRequestsMatrixB <- createRequestsMatrix(
        locationNodes = locationNodes,
        commentRequests = comparisonRequestsB |> dplyr::filter(State == state, CommentID == commentID)
      )
      
      ## create comparison requests matrix a ----
      groundTruthGroupedEdges <- groundTruthRequestsMatrix[["Groupings"]][
        upper.tri(groundTruthRequestsMatrix[["Groupings"]])] |> as.logical()
      groundTruthSeparatedEdges <- groundTruthRequestsMatrix[["Separations"]][
        upper.tri(groundTruthRequestsMatrix[["Separations"]])] |> as.logical()
      groupedEdgesA <- comparisonRequestsMatrixA[["Groupings"]][
        upper.tri(comparisonRequestsMatrixA[["Groupings"]])] |> as.logical()
      separatedEdgesA <- comparisonRequestsMatrixA[["Separations"]][
        upper.tri(comparisonRequestsMatrixA[["Separations"]])] |> as.logical()
      groupedEdgesB <- comparisonRequestsMatrixB[["Groupings"]][
        upper.tri(comparisonRequestsMatrixB[["Groupings"]])] |> as.logical()
      separatedEdgesB <- comparisonRequestsMatrixB[["Separations"]][
        upper.tri(comparisonRequestsMatrixB[["Separations"]])] |> as.logical()
      
      ## compile edges table for grouped edges ----
      groupedAccuracyTable <- dplyr::tibble(
        State = state,
        CommentID = commentID,
        `Edge Type` = "grouped",
        `Ground Truth Edges` = groundTruthGroupedEdges,
        `Comparison Edges A` = groupedEdgesA,
        `Comparison Edges B` = groupedEdgesB
      )
      
      ## compile edges table for separated edges ----
      separatedAccuracyTable <- dplyr::tibble(
        State = state,
        CommentID = commentID,
        `Edge Type` = "separated",
        `Ground Truth Edges` = groundTruthSeparatedEdges,
        `Comparison Edges A` = separatedEdgesA,
        `Comparison Edges B` = separatedEdgesB
      )
      
      ## combine edges tables ----
      commentAccuracyTable <- dplyr::bind_rows(
        groupedAccuracyTable,
        separatedAccuracyTable
      )
      
      ## return comment accuracy table ----
      return(commentAccuracyTable)
    }
  )
  
  # return request level accuracy data ----
  return(accuracyData)
}


createRequestsMatrix <- function(locationNodes, commentRequests) {
  
  # initialize empty groupings matrix ----
  groupingsMatrix <- matrix(
    data = 0,
    nrow = length(locationNodes),
    ncol = length(locationNodes),
    dimnames = list(locationNodes, locationNodes)
  )
  
  # initialize empty separations matrix ----
  separationsMatrix <- matrix(
    data = 0,
    nrow = length(locationNodes),
    ncol = length(locationNodes),
    dimnames = list(locationNodes, locationNodes)
  )
  
  # add matrix elements for pairs of grouped and separated nodes ----
  purrr::walk2(
    .x = commentRequests |> dplyr::pull(Groupings),
    .y = commentRequests |> dplyr::pull(Separations),
    .f = \(commentGrouping, commentSeparation) {
      groupingNodes <- commentGrouping |> dplyr::pull(Location)
      separatedNodes <- commentSeparation |> dplyr::pull(Location)
      groupedEdges <- groupingNodes |> tidyr::crossing(groupingNodes) |> as.matrix()
      separatedEdges <- groupingNodes |> tidyr::crossing(separatedNodes) |> as.matrix()
      groupingsMatrix[groupedEdges] <<- groupingsMatrix[groupedEdges] + 1
      separationsMatrix[separatedEdges] <<- separationsMatrix[separatedEdges] + 1
    }
  )
  
  # compile requests matrices for groupings and separations ----
  requestsMatrix <- list(Groupings = groupingsMatrix, Separations = separationsMatrix)
  
  # return requests matrix ----
  return(requestsMatrix)
}