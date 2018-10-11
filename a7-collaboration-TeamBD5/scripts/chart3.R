library(ggplot2)
library(dplyr)

major_git_plot <- function(survey_data) {
  ggplot(data = survey_data, aes(
    x = (factor(unlist(survey_data[5]))),
    fill = factor(unlist(survey_data[2])))
  ) +
    geom_bar(stat = "count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Experience Level") +
    ylab("Count") +
    ggtitle("R experience by major") +
    labs(fill = "Major Decision") +
    scale_x_discrete(labels = c("No response", "Expert user",
                                "Used it a few times",
                                "Intermediate user", "Never used it"
                               )
                    )
}