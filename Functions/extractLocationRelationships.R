
extractLocationRelationships <- function(
    comment,
    locationNames,
    districtTypes = c(
      "State House",
      "State Senate",
      "Congressional",
      "NA"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.character(comment))
  stopifnot(all(is.character(locationNames)))
  match.arg(districtTypes, several.ok = TRUE)
  stopifnot(is.character(model))

  # initialize chat object ----
  chat <- ellmer::chat_openrouter(model = model)

  # extract comment location relationships ----
  relationships <- chat$chat_structured(
    comment,
    type = ellmer::type_array(
      description = ellmer::interpolate(
        "This commenter is providing public input about a legislative redistricting cycle, 
        requesting that the geographic locations they mention either be kept together or separated 
        into various districts based on communities of interest.
        Return any pairwise location relationships requested by the commenter.
        Interpret the commenter's request, complaint, or concern.
        Do not treat the composition of an existing or proposed district as a desired relationship 
        unless the commenter explicitly endorses or rejects that composition.
        
        The following are examples of a grouped pairwise relationship between 'Springfield' and 'Washington County':
        'Vote no on the proposed map, which separates Springfield from Washington County.',
        'Springfield and Washington County have been separated. This is a clear effort to Gerrymander our community.',
        'Springfield and Washington County share similar economic interests.',
        'Why is Springfield in a different district than Washington County?',
        'Separating Springfield from Washington County is ridiculous.',
        'Please Keep Springfield together with Washington County.',
        'Don't split Springfield from Washington County.', and
        'Springfield should be reunited with Washington County.'.
        
        The following are examples of a separated pairwise relationship between 'Springfield' and 'Washington County':
        'Vote no on the proposed map, which combines Springfield with Washington County.',
        'Springfield and Washington County have been combined. This is a clear effort to Gerrymander our community.',
        'Springfield and Washington County do not share similar economic interests.',
        'Why is Springfield in the same district as Washington County?',
        'Combining Springfield and Washington County is ridiculous',
        'Please Keep Springfield separated from Washington County.',
        'Don't group Springfield with Washington County.', and
        'Springfield should be kept apart from Washington County.'."
      ),
      items = ellmer::type_object(
        Location1 = ellmer::type_enum(values = locationNames),
        Location2 = ellmer::type_enum(values = locationNames),
        Relationship = ellmer::type_enum(
          description = ellmer::interpolate("Return the nature of the relationship between the two locations"),
          values = c("grouped", "separated", "unclear")),
        DistrictType = ellmer::type_enum(
          description = ellmer::interpolate(
            "Return the legislative district type relevant to this pairwise relationship request,
            if mentioned by the commenter. If the relevant district types are unclear or if multiple types 
            are relevant, return 'NA'"
          ),
          values = districtTypes
        ),
        Confidence = ellmer::type_enum(values = c("low", "medium", "high"))
      )
    )
  )

  # return comment location relationships ----
  return(relationships)
}
