screenCommentsForCOIs <- function(prompts, state, model = "mistralai/mistral-large") {
  
  # check arguments ----
  stopifnot(is.list(prompts))
  stopifnot(state %in% state.name)
  
  ## initialize chat object ----
  chat <- ellmer::chat_openrouter(model = model)
  
  # evaluate whether description mentions a community of interest ----
  coiIndicators <- ellmer::parallel_chat_structured(
    chat = ellmer::chat_openrouter(model = model),
    prompts = prompts,
    type = ellmer::type_integer(
      description = ellmer::interpolate(
        "Binary indicator measuring the specificity of this
        {{state}} commenter's description of a community of interest.
        A value of 1 indicates that the commenter is clearly discussing particular,
        named locations that they want kept together or separate for a district map.
        A value of 0 indicates that the commenter fails to mention specific locations
        or that they are discussing a different redistricting concern.",
        state = state
      )
    ),
    convert = TRUE,
    include_tokens = TRUE,
    include_cost = TRUE,
    max_active = 10,
    rpm = 500,
    on_error = "continue"
  )
  
  # return community of interest indicators ----
  return(coiIndicators)
}