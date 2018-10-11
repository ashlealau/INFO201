library(httr)
library(dplyr)
library(jsonlite)
library(stringr)

# Set up
api_url <- "https://www.googleapis.com/civicinfo/v2/representatives/"
source("api-keys.R")
chosen_address <- "4522 18th NE Ave, WA"
state <- str_sub(chosen_address, -2, -1)
params <- list(address = chosen_address, key = civic_info_key)
# Make a request
response <- GET(api_url, query = params)
body <- content(response, "text")
parsed_data <- fromJSON(body)

# Data wrangling (assuming that `parsed_data` is the parsed JSON response)
offices <- parsed_data$offices
officials <- parsed_data$officials

# Expand officies by the number of elements in the `indices` column
# See: https://stackoverflow.com/questions/2894775/replicate-each-row-of-data-frame-and-specify-the-number-of-replications-for-each
num_to_rep <- unlist(lapply(parsed_data$offices$officialIndices, length))
expanded <- offices[rep(row.names(offices), num_to_rep), ]
officials <- mutate(officials, index = row_number() - 1)
expanded <- expanded %>% mutate(index = row_number() - 1) %>%
  rename(position = name)

# Then, extracting the relevant columns
offices <- select(expanded, position)

# Flattening for clarity not out of neccessity, makes code easier to read/manipulate
officials <- flatten(select(officials, name, party, emails,  phones, photoUrl, party))

# Format pictures so that they show when available
officials$photoUrl <- paste0("<img src = ", dQuote(officials$photoUrl), ">")

# Url can't be shortened because knitr can't access shortened urls, and 
# the url itself is over 80 characters so it can't be helped
officials$photoUrl <- gsub(
                        "NA", "https://vignette.wikia.nocookie.net/pandorahearts/images/a/ad/Not_available.jpg/revision/latest?cb=20141028171337",
                        officials$photoUrl
                      )

#Format emails so they show "none" when there is no email
officials$emails <- paste0("<", officials$emails, ">")
officials$emails <- gsub("<NULL>", "Not available", officials$emails)


# Format urls so that they are attatched to names
officials$name <- paste0("[", officials$name, "]", "(", officials$urls, ")")
civic_data_frame <- data.frame(officials, offices) %>%
  rename(
    Position = position, Party = party,
    Name = name, Photo = photoUrl,
    Phone = phones, Email = emails
  )