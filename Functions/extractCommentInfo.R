extractCommentInfo <- function(
    prompts,
    localContext,
    stateRegions,
    adminLevels = c(
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
    subareaDescriptions = c(
      "Northern",
      "Northeastern",
      "Eastern",
      "Southeastern",
      "Southern",
      "Southwestern",
      "Western",
      "Northwestern",
      "Central",
      "Unincorporated",
      "Rural",
      "Suburban",
      "Urban",
      "Downtown",
      "NA"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.list(prompts))
  stopifnot(is.character(localContext))
  stopifnot(all(is.character(stateRegions)))
  match.arg(adminLevels, several.ok = TRUE)
  match.arg(subareaDescriptions, several.ok = TRUE)
  stopifnot(is.character(model))
  
  # extract comment information ----
  commentInfo <- ellmer::parallel_chat_structured(
    chat = ellmer::chat_openrouter(
      system_prompt = ellmer::interpolate(
        "You are an expert geocoding research assistant evaluating public comments from the
        2021-2022 redistricting cycle in the United States for quantitative downstream
        evaluation of institutional compliance with constituent input.
        The goal is to extract consistently named and formatted locations and
        comment-level metadata. The provided comments were all written by concerned citizens
        from {{localContext}}.",
        localContext = localContext
      ),
      model = model
    ),
    prompts = prompts,
    type = ellmer::type_object(
      
      ## locations included in the community of interest ----
      LocationsMentioned = ellmer::type_array(
        description = ellmer::interpolate(
          "All individual geographic locations that this commenter mentions,
          including landmarks, neighborhoods, townships, boroughs, towns, cities, school districts, counties, 
          legislative districts, and regions. Only return clearly-identified locations in the commenter's 
          home state relevant to the commenter's community of interest. 
          Order locations by their appearance in the comment."
        ),
        items = ellmer::type_object(
          
          ### location admin level -----
          AdminLevel = ellmer::type_enum(
            values = adminLevels,
            description = ellmer::interpolate(
              "Best estimate of the type or administrative level of the location. Must be exclusive and decisive.
              If the location's administrative level is unclear, return 'NA'."
            )
          ),
          
          ### location name ----
          Name = ellmer::type_string(
            description = ellmer::interpolate(
              "After finding the location's administrative level, return
              the canonical, administrative name of the location.
              Unless the location is a region, do not include any subareas in the name
              (i.e. return 'Washington County' if the comment mentions 
              'Northern Washington County' or 'Rural Washington County').
              Do not return proposed legislative districts, only existing districts.
              Location names should thus read like the following rules:
              
              Naming rules:\n
              - Landmark: use the landmark's canonical name.\n
              - Neighborhood: use the neighborhood's canonical name.\n
              - Township: use the municipality's canonical name.\n
              - Borough: use the municipality's canonical name.\n
              - Town: use the municipality's canonical name.\n
              - City: use the municipality's canonical name.\n
              - School District: use '[Name] School District'.\n
              - County: use '[Name] County'.\n
              - State House District: use 'State House District [Number]'.\n
              - State Senate District: use 'State Senate District [Number]'.\n
              - Congressional District: use 'Congressional District [Number]'.\n
              - Region: Name must exactly match one of the permitted region names.\n\n
              Permitted region names: {{stateRegions}}",
              stateRegions = paste(stateRegions, collapse = ", ")
            )
          ),
          
          ### cardinal direction subarea extent ----
          SubareaDescription = ellmer::type_enum(
            required = FALSE,
            values = subareaDescriptions,
            description = ellmer::interpolate(
              "A subarea specific to the location, if explicitly mentioned by the commenter.
              If the commenter refers to the entirety of the location (i.e. 'Washington County'), return 'NA'.
              Proper names of a location or region (i.e. 'Westridge', 'South Bend', 'Northern California')
              do not imply a separate subarea mention. Return 'NA' in these cases."
            )
          )
        )
      )
    )
  )
  
  # return comment information ----
  return(commentInfo)
}