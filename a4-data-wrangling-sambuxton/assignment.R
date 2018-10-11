# a4-data-wrangling

# Before you get started, set your working directory using the Session menu.
# While we (mostly) don't require specific variable names, we will be checking
# your code (structure + style) as well as your output. The .csv files you save 
# must have the described format/column names, and the file name provided. 
# For all .csv file, make sure to exclude rownames, and write them to the 
# a folder called `output/` which you will create below.

################################### Set up ###################################

# Install (if not installed) + load dplyr package 
library("dplyr")

# Read in `any_drinking.csv` data using a *relative path*
any_drinking <- read.csv("./data/any_drinking.csv")

# Read in `binge.drinking.csv` data using a *relative path*
binge_drinking <- read.csv("./data/binge_drinking.csv")

# Create a directory (using R) called "output" in your project directory
# Make sure to *suppress any warnings*, in case the directory already exists
# You must save all .csv files in this directory (last reminder!)
dir.create("./output", showWarnings = FALSE)

############################# Any drinking in 2012 #############################

# For this first section, you will work only with the *any drinking* dataset.
# In particular, we'll focus on data from 2012. All output should include only 
# the relevant 2012 columns (as well as `state` + `location`), described below. 


# Create a new data.frame that has the `state` and `location` columns, 
# and all columns with data from 2012. you will use this dataframe throughout 
# the rest of this section.
state_location <- select(
                    any_drinking, location, state,
                    both_sexes_2012, males_2012,
                    females_2012
                  )

# Using the (new) 2012 data, create a column `diff` that has 
# the difference in male and female drinking rates
state_location <- mutate(state_location, diff = males_2012 - females_2012)

# Write your data to a file `diff_2012.csv` (in your `output/` directory)
# Make sure to exclude rownames (for all .csv files! -- last reminder).
write.csv(state_location, file = "./output/diff_2012.csv", row.names = FALSE)

# To answer "Are there any locations where females drink more than males"?
# Create a new dataframe by filtering the 2012 dataframe to the rows that 
# meet the criterion. Keep only the `state`, `location`, and column of interest. 
# Write your answer to `more_m_than_f.csv`.
# The answer is no, data frame is empty
# By 2012 data frame do they mean the change one?
more_m_than_f <- select(state_location, state, location, diff) %>%
  filter(diff < 0)

write.csv(more_m_than_f, file = "./output/more_m_than_f.csv", row.names = FALSE)

# To answer the question: "What is the location in which male and female 
# drinking rates are most similar", create a new dataframe by filtering the 2012 
# dataframe to the rows that meet the criterion. Keep only the `state`, 
# `location`, and column of interest. Write your answer to `most_similar.csv`.
# What do they mean by rows
most_similar <- select(state_location, location, state, diff) %>%
  filter(diff == min(abs(diff)))
write.csv(most_similar, file = "./output/most_similar.csv", row.names = FALSE)

# As you've (hopefully) noticed, the `location` column includes national, 
# state, and county level estimates. However, many audiences may only be 
# interested in the *state* level data. Given that, you should do the following:
# Create a new data frame that is only the *state level* observations in 2012.
# For the sake of this analysis, you should treat Washington D.C. as a *state*
# Write this data frame to `state_only.csv`.
# I noticed that counties always match the location column
state_only <- filter(
                state_location, as.character(location) == as.character(state)
              )
write.csv(state_only, file = "./output/state_only.csv", row.names = FALSE)

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest
# Write this data frame to `highest_state.csv`.
highest_state <- select(state_only, state, both_sexes_2012) %>%
  filter(both_sexes_2012 == max(both_sexes_2012))
write.csv(highest_state, file = "./output/highest_state.csv", row.names = FALSE)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest
# Write this data frame to `lowest_state.csv`.
lowest_state <- select(state_only, state, both_sexes_2012) %>%
  filter(both_sexes_2012 == min(both_sexes_2012))
write.csv(lowest_state, file = "./output/lowest_state.csv", row.names = FALSE)

# What was the difference in prevalence between the state with the highest level
# of consumption,and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)
# Store your answer in a variable called `biggest_state_diff`.
biggest_state_diff <- highest_state$both_sexes_2012 -
                        lowest_state$both_sexes_2012


# Write a function called `get_state_data` that allows you to specify a state, 
# then saves a .csv file (`STATE_data.csv`) with observations from that state 
# This includes data about the state, as well as the counties in the state
# You should use the full any.drinking dataset in this function (not just 2012)
get_state_data <- function(state_search) {
  state_name <- filter(any_drinking, state == state_search)
  write.csv(state_name, file = paste0(
                                 "./output/", state_search,
                                 ".csv"), row.names = FALSE
                               )
}

# Demonstrate that you function works by passing "Utah" to the function
get_state_data("Utah")

############################ Binge drinking Dataset ############################

# In this section, you will ask a variety of questions regarding the 
# `binge_drinking.csv` dataset. More specifically, you will analyze a subset of 
# the observations of *just the counties* (exclude state/national estimates!).
# You will store your answers in a *named list*, and at the end of the section, 
# Convert that list to a data frame, and write the data frame to a .csv file.
# Pay close attention to the *names* to be used in the list.


# Create a dataframe with only the county level observations from the 
# `binge_driking.csv` dataset. You should (again) think of Washington D.C. as 
# a state, and therefore *exclude it here*.
# However, you should include "county-like" areas such as parishes and boroughs
county_only <- filter(
                 binge_drinking,
                 as.character(location) != as.character(state)
               ) %>%
                 filter(location != "United States")

# Create an empty list in which to store answers to the questions below.
answer_list <- list()

# What is the average county level of binge drinking in 2012 for both sexes?
# Store the number in your list as `avg_both_sexes`.
answer_list$avg_both_sexes <- mean(county_only$both_sexes_2012)

# What is the name of the county with the largest increase in male binge 
# drinking between 2002 and 2012?
# Store the county name in your list as `largest_male_increase`.
largest_male_increase <- filter(
                           county_only,
                           males_2012 - males_2002 == 
                             max(males_2012 - males_2002)
                           ) %>%
                             select(location)
answer_list$largest_male_increase <- largest_male_increase[1]

# How many counties experienced an increase in male binge drinking between
# 2002 and 2012?
# Store the number in your list as `num_male_increase`.
counties_male_increase <- filter(county_only, (males_2012 - males_2002) > 0)
answer_list$num_male_increase <- nrow(counties_increase)

# What fraction of counties experienced an increase in male binge drinking 
# between 2002 and 2012?
# Store the fraction (num/total) in your list as `frac_male_increase`.
# I could have just used the number of rows in county only but, figured this solution was more robust
answer_list$frac_male_increase <- nrow(counties_increase) / nrow(county_only)
                    
# How many counties experienced an increase in female binge drinking between
# 2002 and 2012?
# Store the number in your list as `num_female_increase`.
counties_female_increase <- filter(
                              county_only,
                              (females_2012 - females_2002) > 0
                            )
answer_list$num_female_increase <- nrow(counties_female_increase)

# What fraction of counties experienced an increase in female binge drinking 
# between 2002 and 2012?
# Store the fraction (num/total) in your list as `frac_female_increase`.
answer_list$frac_female_increase <- (nrow(counties_female_increase)
                                       / nrow(county_only))

# How many counties experienced a rise in female binge drinking *and* 
# a decline in male binge drinking?
# Store the number in your list as `num_f_increase_m_decrease`.
answer_list$num_f_increase_m_decrease <- nrow(
                                           filter(counties_female_increase,
                                           (males_2012 - males_2002) < 0)
                                         )

# Convert your list to a data frame, and write the results 
# to the file `binge_info.csv`
write.csv(
  data.frame(answer_list),
  file = "./output/binge_info.csv",
  row.names = FALSE
)

# The next questions return *data frames as results*:

# What is the *minimum* level of binge drinking in each state in 2012 for 
# both sexes (across the counties)? Your answer should contain roughly 50 values
# (one for each state), unless there are two counties in a state with the 
# same value. Your answer should be a *dataframe* with the location, state, and 
# 2012 binge drinking rate. Write this to a file called `min_binge.csv`.


min_binge <- group_by(county_only, state) %>%
              filter(both_sexes_2012 == min(both_sexes_2012)) %>%
              select(state, location, both_sexes_2012)

write.csv(
  data.frame(min_binge),
  file = "./output/min_binge.csv",
  row.names = FALSE
)

# What is the *maximum* level of binge drinking in each state in 2012 for 
# both sexes (across the counties)? Your answer should contain roughly 50 values
# (one for each state), unless there are two counties in a state with the 
# same value. Your answer should be a *dataframe* with the location, state, and 
# 2012 binge drinking rate. Write this to a file called `max_binge.csv`.

max_binge <- group_by(county_only, state) %>%
  filter(both_sexes_2012 == max(both_sexes_2012)) %>%
  select(state, location, both_sexes_2012)

write.csv(
  data.frame(max_binge),
  file = "./output/max_binge.csv",
  row.names = FALSE
)
################################# Joining Data #################################
# You'll often have to join different datasets together in order to ask more 
# involved questions of your dataset. In order to join our datasets together, 
# you'll have to rename their columns to differentiate them. 


# First, rename all prevalence columns in the any_drinking dataset to the 
# have prefix "any_" (i.e., `males_2002` should now be `any_males_2002`)
# Hint: you can get (and set!) column names using the colnames function. 
# This may take multiple lines of code.
colnames(any_drinking)[3:35] <- paste0("any_", colnames(any_drinking)[3:35])

# Then, rename all prevalence columns in the binge_drinking dataset to the have
# the prefix "binge_" (i.e., `males_2002` should now be `binge_males_2002`)
# This may take multiple lines of code.
colnames(binge_drinking)[3:35] <- paste0(
                                    "binge_",
                                    colnames(binge_drinking)[3:35])

# Then, create a dataframe by joining together the both datasets. 
# Think carefully about the *type* of join you want to do, and what the 
# *identifying columns* are. You will use this (joined) data to answer the 
# questions below.
joined_drinking <- left_join(binge_drinking, any_drinking)

# Create a column `diff_2012` storing the difference between `any` and `binge` 
# drinking for both sexes in 2012
joined_drinking <- mutate(
                     joined_drinking,
                     diff_2012 = (any_both_sexes_2012 - binge_both_sexes_2012)
                   )

# Which location has the greatest *absolute* difference between `any` and 
# `binge` drinking? Your answer should be a one row data frame with the state, 
# location, and column of interest (diff_2012). 
# Write this dataframe to `biggest_abs_diff_2012.csv`.
biggest_abs_diff_2012 <- filter(
                           joined_drinking, diff_2012 == max(abs(diff_2012))
                         )
write.csv(
  data.frame(biggest_abs_diff_2012),
  file = "./output/biggest_abs_diff_2012.csv",
  row.names = FALSE
)


# Which location has the smallest *absolute* difference between `any` and 
# `binge` drinking? Your answer should be a one row data frame with the state, 
# location, and column of interest (diff_2012). 
# Write this dataframe to `smallest_abs_diff_2012.csv`.
smallest_abs_diff_2012 <- filter(
                            joined_drinking,
                            diff_2012 == min(abs(diff_2012))
                          )
write.csv(
  data.frame(smallest_abs_diff_2012),
  file = "./output/smallest_abs_diff_2012.csv",
  row.names = FALSE
)

############## Write a function to ask your own question(s) ####################
# Even in an entry level data analyst role, people are expected to come up with 
# their own questions of interest (not just answer the questions that other 
# people have). For this section, you should *write a function* that allows you 
# to ask the same question on different subsets of data. For example, you may 
# want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and 
# function name. After writing your function, *demonstrate* that the function 
# works by passing in different parameters to your function.
# My question is "What is the location with the highest drinking level given a particular year?
highest_drinking_male <- function (year) {
  index <- 3 * (year - 2002) + 5
  highest_drinking <- filter(
                        any_drinking,
                        any_drinking[index] == max(any_drinking[index])
                      )
  highest_drinking[1, 2]
}
highest_drinking_male(2012)
################################### Challenge ##################################

# Using your function from part 1 that wrote a .csv file given a state name, 
# write a separate file for each of the 51 states (including Washington D.C.)
# The challenge is to do this in a *single line of (very concise) code*
lapply(any_drinking$state, get_state_data)

# Write a function that allows you to pass in a *dataframe* (i.e., in the format 
# of binge_drinking or any_drinking) *year*, and *state* of interest. The 
# function should saves a .csv file with observations from that state's counties
# (and the state itself). It should only write the columns `state`, `location`, 
# and data from the specified year. Before writing the .csv file, you should 
# *sort* the data.frame in descending order by the both_sexes drinking rate in 
# the specified year. The file name should have the format:
# `DRINKING_STATE_YEAR.csv` (i.e. `any_Utah_2005.csv`).
# To write this function, you will either have to use a combination of dplyr 
# and base R, or confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
csv_state_year <- function(dataframe, year, state_name) {
  drinking_type <- gsub("_.*", "", colnames(dataframe)[3])
  dataframe_sorted <- select_(
                        dataframe, "state", "location",
                        paste0(drinking_type, "_males_", year),
                        paste0(drinking_type, "_females_", year),
                        paste0(drinking_type, "_both_sexes_", year)
                      ) %>%
                        arrange_(paste0(
                                   "-", drinking_type,
                                   "_both_sexes_", year)
                                 ) %>%
                                  filter(state == state_name)
  write.csv(
    dataframe_sorted,
    file = paste0(
             "./output/", drinking_type,
             "_", state_name, "_", year, ".csv"
           )
  )
}

# Create the file `binge_Colorado_2007.csv` using your function.
csv_state_year(binge_drinking, "2007", "Colorado")