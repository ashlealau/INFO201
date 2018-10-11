library(dplyr)
library(ggplot2)

chart_1 <- function(dataset) {
  ## Reading the data set and making it a data frame
  dataset <- read.csv(file = "data/intro-survey.csv")
  dataset_frame <- dataset

  ## Selecting items to Graph and Renaming Columns
  items_to_graph <- dataset_frame %>%
    select(What.is.your.current.class.standing.,
           Were.you.born.in.Washington.state.) %>%
    rename(Class = What.is.your.current.class.standing.) %>%
    rename(Born_In_Washington = Were.you.born.in.Washington.state.)

  ## Counting the total students by taking the length
  ## of Class column
  total_students <- length(items_to_graph$Class)

  ## Plotting data onto a bar graph
  plot <- ggplot(items_to_graph, aes(x = Born_In_Washington,
                                     y = total_students)) +
    geom_bar(stat = "identity", width = .5, fill = "tomato3") +
    labs(
      title = "Students Born in Washington",
      subtitle = "Students Born in Washington vs Not Born in Washington",
      caption = "source: intro-survey.csv"
    )
  plot + labs(x = "Born In Washington", y = "Total Students")

  return(plot)
}