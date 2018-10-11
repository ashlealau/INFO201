library(dplyr)

intro_survey <- read.csv("./data/intro-survey.csv", stringsAsFactors = FALSE)

intro_function <- function(dataset) {
  ret <- list()
  ret$class_standing <- select(intro_survey,
                        "What.is.your.current.class.standing.") %>%
                        group_by(What.is.your.current.class.standing.) %>%
                        tally()
  ret$freshman <- ret$class_standing[2, 2]
  ret$sophomore <- ret$class_standing[5, 2]
  ret$junior <- ret$class_standing[3, 2]
  ret$senior <- ret$class_standing[4, 2]
  ret$blank <- ret$class_standing[1, 2]
  return(ret)
}