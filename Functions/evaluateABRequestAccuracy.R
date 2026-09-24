
evaluateABRequestAccuracy <- function(
    groundTruthLocations,
    groundTruthRequests,
    comparisonRequestsA,
    comparisonRequestsB
  ) {
  
  # conduct full suite of regression tests for recall and precision ----
  accuracyTable <- dplyr::bind_rows(
    evaluateABRequestRecall(
      groundTruthLocations = groundTruthLocations,
      groundTruthRequests = groundTruthRequests,
      comparisonRequestsA = comparisonRequestsA,
      comparisonRequestsB = comparisonRequestsB,
      unit = "location pair"
    ),
    evaluateABRequestRecall(
      groundTruthLocations = groundTruthLocations,
      groundTruthRequests = groundTruthRequests,
      comparisonRequestsA = comparisonRequestsA,
      comparisonRequestsB = comparisonRequestsB,
      unit = "comment"
    ),
    evaluateABRequestPrecision(
      groundTruthLocations = groundTruthLocations,
      groundTruthRequests = groundTruthRequests,
      comparisonRequestsA = comparisonRequestsA,
      comparisonRequestsB = comparisonRequestsB,
      unit = "location pair"
    ),
    evaluateABRequestPrecision(
      groundTruthLocations = groundTruthLocations,
      groundTruthRequests = groundTruthRequests,
      comparisonRequestsA = comparisonRequestsA,
      comparisonRequestsB = comparisonRequestsB,
      unit = "comment"
    )
  )
  
  # return accuracy table ----
  return(accuracyTable)
}

evaluateABRequestRecall <- function(
    groundTruthLocations,
    groundTruthRequests,
    comparisonRequestsA,
    comparisonRequestsB,
    unit = c("location pair", "comment")
  ) {
  
  # check arguments ----
  unit <- match.arg(arg = unit)
  
  # compile request level accuracy table ----
  requestAccuracy <- calculateRequestAccuracy(
    groundTruthLocations = groundTruthLocations,
    groundTruthRequests = groundTruthRequests,
    comparisonRequestsA = comparisonRequestsA,
    comparisonRequestsB = comparisonRequestsB
  )
  
  # calculate recall model statistics based on unit of aggregation ----
  if (unit == "location pair") {
    
    ## build location-pair level recall contrast ----
    matchedRequests <- requestAccuracy |>
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
    matchedRequests <- requestAccuracy |>
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


evaluateABRequestPrecision <- function(
    groundTruthLocations,
    groundTruthRequests,
    comparisonRequestsA,
    comparisonRequestsB,
    unit = c("location pair", "comment")
) {
  
  # check arguments ----
  unit <- match.arg(arg = unit)
  
  # compile request level accuracy table ----
  requestAccuracy <- calculateRequestAccuracy(
    groundTruthLocations = groundTruthLocations,
    groundTruthRequests = groundTruthRequests,
    comparisonRequestsA = comparisonRequestsA,
    comparisonRequestsB = comparisonRequestsB
  )
  
  # calculate recall model statistics based on unit of aggregation ----
  if (unit == "location pair") {
    
    ## build location-pair level precision data frames ----
    matchedRequestsA <- requestAccuracy |>
      dplyr::filter(`Comparison Edges A`) |>
      dplyr::mutate(Method = "A", Matched = `Ground Truth Edges` & `Comparison Edges A`) |>
      dplyr::select(State, CommentID, Method, Matched)
    matchedRequestsB <- requestAccuracy |>
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
    matchedRequestsA <- requestAccuracy |>
      dplyr::filter(`Comparison Edges A`) |>
      dplyr::mutate(Method = "A", Matched = `Ground Truth Edges` & `Comparison Edges A`) |>
      dplyr::select(State, CommentID, Method, Matched) |>
      dplyr::group_by(State, CommentID, Method) |>
      dplyr::summarise(Precision = sum(Matched)/dplyr::n())
    matchedRequestsB <- requestAccuracy |>
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

calculateRequestAccuracy <- function(
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
  
  # compile request-level matrix accuracy across all states and comments ----
  requestAccuracy <- purrr::map2_dfr(
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
      groundTruthEdges <- groundTruthRequestsMatrix[upper.tri(groundTruthRequestsMatrix)] |> as.logical()
      comparisonEdgesA <- comparisonRequestsMatrixA[upper.tri(comparisonRequestsMatrixA)] |> as.logical()
      comparisonEdgesB <- comparisonRequestsMatrixB[upper.tri(comparisonRequestsMatrixB)] |> as.logical()
      
      ## compile comment accuracy table ----
      commentAccuracyTable <- dplyr::tibble(
        State = state,
        CommentID = commentID,
        `Ground Truth Edges` = groundTruthEdges,
        `Comparison Edges A` = comparisonEdgesA,
        `Comparison Edges B` = comparisonEdgesB,
      )
      
      ## return comment accuracy table ----
      return(commentAccuracyTable)
    }
  )
  
  # return request level accuracy table ----
  return(requestAccuracy)
}

createRequestsMatrix <- function(locationNodes, commentRequests) {
  
  # initialize empty requests matrix ----
  requestsMatrix <- matrix(
    data = 0,
    nrow = length(locationNodes),
    ncol = length(locationNodes),
    dimnames = list(locationNodes, locationNodes)
  )
  
  # add matrix elements for each pair of grouped location nodes ----
  purrr::walk(
    .x = commentRequests |> dplyr::pull(Groupings),
    .f = \(commentGrouping) {
      groupingNodes <- commentGrouping |> dplyr::pull(Location)
      locationEdges <- groupingNodes |> expand.grid(groupingNodes) |> as.matrix()
      requestsMatrix[locationEdges] <<- requestsMatrix[locationEdges] + 1
    }
  )
  
  # return requests matrix ----
  return(requestsMatrix)
}