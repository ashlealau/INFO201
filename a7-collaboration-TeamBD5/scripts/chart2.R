# Setup
library(dplyr)
library(ggplot2)

chart_2 <- function(dataset) {
  # Constructing the dataframe with relevant columns
  survey_data <- dataset %>%
    select(What.is.your.current.class.standing.,
           How.many.siblings.do.you.have.) %>%
    rename(Class_Standing = What.is.your.current.class.standing.) %>%
    rename(Num_Siblings = How.many.siblings.do.you.have.) %>%
    filter(Class_Standing != "")
  survey_data$Class_Standing <- unlist(survey_data$Class_Standing)

  # Plotting a box and whisker plot using the dataframe
  data_plot <- ggplot(survey_data, aes(x = Class_Standing,
                                       y = Num_Siblings,
                                       color = Class_Standing)) +
    geom_boxplot() +
    labs(title = "Class Standing v.s. Number of Siblings",
         x = "Class Standing",
         y = "Number of Siblings",
         colour = "Class Standing")
  return(data_plot)
}