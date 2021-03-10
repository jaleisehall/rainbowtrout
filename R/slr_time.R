#' @title slr_time function
#' 
#' @param sea_level 
#' @param elevation 
#'
#' @return year
#' @export 
#' @description Calculates time required for inundation given rate of sea level rise. 
#' Sea level rise time function (while loop ESM262 HW3). 
#' Returns year that a city will be inundated given a 3.6mm annual sea level rise (slr) rate (2021 global average). 
#' Takes starting local mean sea level (m) and city elevation (m) as inputs.
#'
#' @examples slr_fun(sea_level = 1.1, elevation = 4.1)
#' [1] "The city will be flooded in year 2105"

slr_time <- function(sea_level, elevation){
  
  # error checking - is city already below sea level?
  elevation = ifelse(
    elevation < sea_level,
    return("City is already flooded"),
    elevation
  )
  
  # set starting year
  year = 2021
  
  # call while loop
  while(sea_level < elevation){
    
    sea_level = sea_level + .036
    year = year + 1
    
  }
  
  # return output of while loop calculation
  return(sprintf("The city will be flooded in year %s", year))
}