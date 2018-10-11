# TODO fix state reference
library(httr)
library(dplyr)
library(jsonlite)
source("civic-info.R")

# Set up
base_url <- "https://api.propublica.org/congress/v1/members/"
chamber <- "house"
endpoint <- paste0(base_url, chamber, "/", state, "/current.json")
api_url <- paste0(base_url, endpoint)
source("api-keys.R")
# Make a request
response <- GET(endpoint, add_headers("X-API-KEY" = propublica_key))
body <- content(response, "text")
parsed_data <- fromJSON(body)
# Formatting data for barplots
gender_num <- c(sum(parsed_data$results[["gender"]] == "M"),
                sum(parsed_data$results[["gender"]] == "F")
               )
par(mai=c(1,1.2,1,1))
# barplot margins and details listed, adjust to all data lengths
barplot(
  horiz = TRUE,
  gender_num, names.arg = c("Males", "Females"),
  xlab = "# of Representatives",
  main = "Representatives by Gender",
  las = 1
)
gender_plot <- recordPlot()

party_num <- c(sum(parsed_data$results[["party"]] == "R"),
               sum(parsed_data$results[["party"]] == "D"))

barplot(
  horiz = TRUE,
  party_num, names.arg = c("Republicans", "Democrats"),
  xlab = "# of Representatives",
  main = "Representatives by Party",
  las = 1
)
party_plot <- recordPlot()
dev.off()
# Selected Representative
# Preparing votes and individual requests
member_id <- parsed_data$results[["id"]][1]
endpoint_member <- paste0(base_url, member_id, ".json")
endpoint_votes <- paste0(base_url, member_id, "/votes.json")

# Individual request
reponse_member <- GET(
                    endpoint_member,
                    add_headers("X-API-KEY" = propublica_key)
                  )
body_member <- content(reponse_member, "text")
parsed_data_member <- fromJSON(body_member)

individual_name <- paste(
          parsed_data_member$results[["first_name"]],
          parsed_data_member$results[["last_name"]]
        )
# Votes request
reponse_votes <- GET(endpoint_votes, add_headers("X-API-KEY" = propublica_key))
body_votes <- content(reponse_votes, "text")
parsed_data_votes <- fromJSON(body_votes)

# Data analysis and organization of individual data
names(parsed_data_member$results)
twitter_account <- parsed_data_member$results[["twitter_account"]]
birth_date <- parsed_data_member$results[["date_of_birth"]]

# Calculation of "age" using the current date
as.Date(Sys.Date(), format = "%d%b%Y") - as.Date(birth_date)
age <- round(as.numeric(
               difftime(as.Date(Sys.Date(), format = "%d%b%Y"),
               (as.Date(birth_date)), units = "weeks") / 52)
             )
# Data analysis and organization of vote information
vote_data <- parsed_data_votes$results[["votes"]]
vote_data_results <- select(data.frame(vote_data), position, result)

# Calculating vote success rate
yes_success <- filter(vote_data_results, position == "Yes") %>%
                 filter(result == "Passed")

agreed_success <- filter(vote_data_results, position == "Yes") %>%
                 filter(result == "Agreed to")

no_success <- filter(vote_data_results, position == "No") %>%
                filter(result == "")
success_rate <- ( ( (nrow(yes_success) + nrow(no_success)) +
                     nrow(agreed_success)) / nrow(vote_data_results))
success_percentage <- paste0(success_rate * 100, "%")
