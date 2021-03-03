## fish counts
## Returns the most common fish, the rarest fish, the total number of fish
## returns plot when requested
## takes vector input of fish species list (or really any list of occurrences)
library(tidyverse)

#' Title
#'
#' @param fish
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
fish_counts <- function(fish, plot = FALSE){

  # error check, ensure that plot input is logical
  plot = ifelse((is.logical(plot) == FALSE),
                return("plot missing 'TRUE', or 'FALSE' argument"), plot)

  # convert input list to factor
  fish <- as.factor(fish)

  # summarize fish
  fish_summary <- summary(fish)

  # find common fish
  common_fish <- which.max(fish_summary)

  # find rare fish
  rare_fish <- which.min(fish_summary)

  # find total number of fish
  total_fish <- sum(fish_summary)

  print(c(common_fish, rare_fish, total_fish))

  # make plot conditionally
  if(plot == TRUE){
    plottitle = sprintf("Here is a figure showing %s fish", sum(fish_summary))

    fish_plot <- ggplot(data.frame(fish),
                        aes(fish, fill=fish))+
      geom_histogram(stat="count")+
      ggtitle(plottitle)+
      labs(x = "Fish species", y = "Count")+
      theme(legend.position = "none")

    return(fish_plot)
  }
}
