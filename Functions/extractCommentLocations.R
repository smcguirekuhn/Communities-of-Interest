extractCommentLocations <- function(
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
    model = "mistral-large-2512"
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
    include_tokens = TRUE,
    chat = ellmer::chat_mistral(
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
          "Return individual geographic locations, including landmarks, neighborhoods, townships, 
          boroughs, towns, cities, school districts, counties, legislative districts, and regions,
          in accordance with the following rules:\n\n
          
          - Return locations that the commenter asks be kept together during redistricting.\n
          - Return locations that the commenter asks be grouped with another location during redistricting.\n
          - Return locations that the commenter asks be separated from another location during redistricting.\n
          - Do not return locations outside the commenter's state.\n
          - Do not return any proposed legislative districts discussed by the commenter,
          only existing districts as of 2020.\n
          - Do not return roads or interstates mentioned by the commenter."
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
              Location names should thus read like the following rules:\n\n
              
              Naming rules:\n
              - Landmark: use the landmark's canonical name.\n
              - Neighborhood: use the neighborhood's canonical name.\n
              - Township: use the municipality's canonical name (do not include 'township' in the name).\n
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
              do not imply a separate subarea mention (i.e. 'Western', 'Southern', and 'Northern').
              Return 'NA' in these cases."
            )
          ),
          
          ### coi request relevance ----
          Relevance = ellmer::type_enum(
            values = c("relevant", "contextual"),
            description = ellmer::interpolate(
              "The relevance of the location to a specific request by the commenter regarding a
              community of interest and a possible legislative district boundary choice.
              Locations should be regarded as 'contextual' when they provide background information
              or anecdotes that are tangential to genuine geographic configurations and the
              broader thesis of the comment. Relevant locations are those that a commenter requests
              be kept whole, grouped with other locations, or separated from other locations.
              The following are examples of when 'contextual' should be returned for a mentioned location.\n\n

              - I was at the meeting in Springfield last night. Here are my observations.\n
              - We need our community better represented in [State Capital City] and Washington DC.\n
              - Our neighborhood has been split, as have many neighborhoods across Washington County.\n
              - I came back to this community after attending college at [State University].\n
              - My comments address several concerns I have over the districts in the western part of the state."
            )
          )
        )
      )
    )
  )
  
  # return comment information ----
  return(commentInfo)
}
