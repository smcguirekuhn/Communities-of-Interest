
addFullLocationNames <- function(commentData) {
  
  # check arguments ----
  stopifnot(is.data.frame(commentData))
  stopifnot(all(c("AdminLevel", "Name", "SubareaDescription") %in% names(commentData)))
  
  # add full location names to comment data ----
  commentData <- commentData |>
    dplyr::mutate(
      FullLocationName = dplyr::case_when(
        SubareaDescription != "NA" ~ paste0(SubareaDescription, " ", Name, " (", AdminLevel, ")"),
        .default = paste0(Name, " (", AdminLevel, ")")
      )
    )
  
  # normalize full location names for repeated words ----
  commentData <- commentData |>
    dplyr::mutate(
      FullLocationName = gsub(
        pattern = "\\b(\\w+)(?:\\s+\\1\\b)+",
        replacement = "\\1",
        x = FullLocationName,
        perl = TRUE
      )
    )
  
  # return comment data ----
  return(commentData)
}