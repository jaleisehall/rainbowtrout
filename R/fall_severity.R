#' @title fall_severity
#'
#' @param fall_length Length of fall in meters or feet. Must match units of rope_length
#' @param rope_length Total length of rope in meters or feet. Must match units of fall_length
#'
#' @return fall severity based on fall factor
#' @export
#'
#' @description Takes inputs fall_length and rope_length to calculate fall factor severity of a climb.
#'
#' @examples fall_severity(fall_length = 10, rope_length = 50) returns: [1] "minor"



fall_severity <- function (fall_length, rope_length){

  #  Error checking to ensure variables greater than 0

  fall_length = ifelse(
    fall_length <= 0,
    return("Fall length must be greater than zero"),
    fall_length
    )


  rope_length=ifelse(
    (rope_length <= 0),
    return("Rope length must be greater than zero"),
    rope_length
  )

  # calculate fall factor based on ratio of fall length to rope length
  fall_factor = fall_length/rope_length

  # classify severity of fall
  fall_severity = case_when(
    fall_factor <= 0.5 ~ "minor",
    fall_factor <= 1 ~ "moderate",
    fall_factor <= 1.5 ~ "severe",
    fall_factor <= 2 ~ "very severe",
    TRUE ~ "oops something went wrong"
  )


  # return fall severity
  return(fall_severity)

}
