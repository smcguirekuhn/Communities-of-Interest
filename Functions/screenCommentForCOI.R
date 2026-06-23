screenCommentForCOI <- function(description, state, model = "mistralai/mistral-large") {
  
  # check arguments ----
  stopifnot(is.character(description))
  stopifnot(state %in% state.name)
  
  ## initialize chat object ----
  chat <- ellmer::chat_openrouter(model = model)
  
  # evaluate whether description mentions a community of interest ----
  coiIndicator <- chat$chat_structured(
    description,
    type = ellmer::type_integer(
      description = glue::glue(
        "Binary indicator measuring the specificity of this",
        "{state} commenter's description of a community of interest.",
        "A value of 1 indicates that the commenter is clearly discussing particular,",
        "named locations that they want kept together or separate for a district map.",
        "A value of 0 indicates that the commenter fails to mention specific locations",
        "or that they are discussing a different redistricting concern."
      )
    )
  )
  
  # return community of interest indicator ----
  return(coiIndicator)
}