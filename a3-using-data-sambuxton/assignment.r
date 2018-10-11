# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### DataFrame Manipulation (20 POINTS) ######################

# Create a vector `first_names` with 5 names in it
first_names <-  c("Sam", "Bert", "Flynn", "Martin", "Betty")

# Create a vector `math_grades` with 5 hypothetical grades (0 - 100)
# in a math course (that correspond to the 5 names above)
math_grades <- c(99, 78, 85, 91, 82)

# Create a vector `spanish_grades` with 5 hypothetical grades (0 - 100)
# in a Spanish course (that correspond to the 5 names above
spanish_grades <- c(85, 95, 78, 84, 85)

# Create a data.frame variable `students` by combining
# the vectors `first_names`, `math_grades`, and `spanish_grades`
students <- data.frame(first_names, math_grades, spanish_grades)

# Create a variable `num_students` that contains the
# number of rows in your dataframe `students`
num_students <- nrow(students)

# Create a variable `num_courses` that contains the number of columns
# in your dataframe `students` minus one (b/c of their names)
num_courses <- ncol(students) - 1

# Add a new column `grade_diff` to your dataframe, which is equal to
# `students$math_grades` minus `students$spanish_grades`
students$grade_diff <- students$math_grades - students$spanish_grades


# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
students$better_at_math <- students$grade_diff > 0

# Create a variable `num_better_at_math` that is the number
# (i.e., one numeric value) of students better at math
num_better_at_math <- sum(students$better_at_math == TRUE)

# Write your `students` dataframe to a new .csv file inside your data/ directory
# with the filename `grades.csv`. Make sure *not* to write row names.
write.csv(students, "grades.csv", row.names = FALSE)

########################### Loading R Data (30 points) #########################

# In this section, you'll work with the `Titanic` data set
# Which is built into the R environment. You should be able to `View()` it
# Pay *close attention* to what each column means. Use ?Titanic to read more.
View(Titanic)

# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# You should convert the `Titanic` variable into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Be sure to **not** treat strings as factors!
Titanic <- data.frame(Titanic, stringsAsFactors = FALSE)

# Create a variable `children` that are the *only* the rows of the data frame
# with information about the children on the Titanic.

children <- Titanic[Titanic["Age"] == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: remember the `sum()` function!
num_children <- sum(Titanic$Age == "Child")

# Create a variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
# Tip: if you want, you can use multiple statements (lines of code)
# if you find that helpful to create this variable.
not_survivors <- Titanic[Titanic["Survived"] == "No", ]
most_lost <- not_survivors[not_survivors["Freq"] == max(not_survivors$Freq), ]

# Define a function called `survival_rate()` that takes in a ticket class
# (e.g., "1st", "2nd") as an argument.This function should return the following
# sentence that compares the *total survival rate* of adult men vs.
# "women and children" in that ticketing class. It should read (for example):
# "Of Crew class, 87% of women and children survived and 22% of men survived.".
# The approach you take to generating the sentence to return is up to you.
# A good solution will likely utilize filtering to produce the required data.
survival_rate <- function(ticket) {
  ticket_rows <- Titanic[Titanic["Class"] == ticket, ]
  adult_rows <- ticket_rows[ticket_rows["Age"] == "Adult", ]
  men_rows <- adult_rows[adult_rows["Sex"] == "Male", ]
  men_dead <- men_rows[men_rows["Survived"] == "No", ]
  men_alive <- men_rows[men_rows["Survived"] == "Yes", ]
  men_mortality <- sum(men_alive$Freq) / (sum(men_dead$Freq)
                   + sum(men_alive$Freq))
  women <- adult_rows[adult_rows["Sex"] == "Female", ]
  children <- ticket_rows[ticket_rows["Age"] == "Child", ]
  not_men <- rbind(children, women)
  not_men_dead <- not_men[not_men["Survived"] == "No", ]
  not_men_alive <- not_men[not_men["Survived"] == "Yes", ]
  not_men_mortality <- sum(not_men_alive$Freq) / (sum(not_men_dead$Freq)
                       + sum(not_men_alive$Freq))
  paste("Of ", ticket, " class ", round(not_men_mortality * 100, 0),
        "% of women and children survived and ", round(men_mortality * 100, 0),
        "% of men survived ", "sep" = "")
}

survival_rate("1st")

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class to your function above.
first_survived <- survival_rate("1st")
second_survive <- survival_rate("2nd")
third_survived <- survival_rate("3rd")
crew_survived <- survival_rate("Crew")


########################### Reading in Data (40 points)#########################
# In this section, we'll read in a .csv file with a tabular row/column layout
# This is like Microsoft Excel or Google Docs, but without the formatting.
# The .csv file we'll be working with has the life expectancy
# for each country in 1960 and 2013. We'll ask real-world questions about the
# data by writing the code that answers our question.

# Using the `read.csv` function, read the life_expectancy.csv file into
# a variable called `life_expectancy`. Makes sure not to read strings as factors

# Assuming that document is already in a3 directory 
life_expectancy <- read.csv("./data/life_expectancy.csv",
                            stringsAsFactors = FALSE)

# Determine if `life_expectancy` is a data.frame by using
# the is.data.frame function. You may also want to View() it.
is.data.frame(life_expectancy)

# Create a column `life_expectancy$change` that is the change
# in life expectancy from 1960 to 2013
life_expectancy$change <- life_expectancy$le_2013 - life_expectancy$le_1960

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy
most_change <- life_expectancy[life_expectancy$change ==
                               max(life_expectancy$change), "country"]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved fewer than 5 years between 1960 and 2013
num_small_gain <- length(life_expectancy[life_expectancy$change < 5, ])

# Write a function `country_change()` that takes in a country's name
# as a parameter, and returns it's change in life expectancy from 1960 to 2013
country_change <- function(name) {
  life_expectancy[life_expectancy$country == name, "change"]
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 2013 in Sweden
sweden_change <- country_change("Sweden")

# Define a function `lowest_life_exp_in_region()` that takes in a **region**
# as an argument, and returns the **name of the country**
# with the lowest life expectancy in 2013 (in that region)
lowest_life_exp_in_region <- function(region) {
  regional_countries <- life_expectancy[life_expectancy$region == region, ]
  regional_countries[regional_countries$change ==
                     min(regional_countries$change), "country"]
}

# Using the function you just wrote, create a variable `lowest_in_south_asia`
# that is the country with the lowest life expectancy in 2013 in South Asia
lowest_in_south_asia <- lowest_life_exp_in_region("South Asia")

# Write a function `bigger_change()` that takes in two country names
# as parameters, and returns a sentence that describes which country experienced
# a larger gain in life expectancy (and by how many years).
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=31.9),
#  whose life expectancy grew by 7.4 years more than Bolivia's (gain=24.5)."
# Make sure to round your numbers to one digit.
bigger_change <- function(name1, name2) {
  larger_change <- max(country_change(name1), country_change(name2))
  larger_change_name <- life_expectancy[life_expectancy$change ==
                                        larger_change, "country"]
  paste("The country with the bigger change in life expectancy was ",
        larger_change_name, " (gain=", larger_change, ")", sep = "")
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- bigger_change("United States", "France")

# Write your `life_expectancy` data.frame to a new .csv file to your
# data/ directory with the filename `life_expectancy_with_change.csv`.
# Make sure not to write row names.
write.csv(life_expectancy, "life_expectancy_with_change.csv", row.names = FALSE)

############################## Challenge (10 points) ###########################
# Create a variable `highest_avg_change` that has the name of the region with
# the highest *average change* in life expectancy between the two time points.
# To do this, you'll need to *compute the average* change across the countries
# in each region, and then compare the averages across regions.
# Feel free to use any library of your choice, or base R functions.
east_asia_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                "East Asia & Pacific"])
middle_east_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                  "Middle East & North Africa"])
europe_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                             "Europe and Central Asia"])
sub_saharan_africa_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                         "Sub-Saharan Africa"])
carribean_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                "Latin American & Carribean"])
south_asia_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                 "South Asia"])
north_america_mean <- mean(life_expectancy$change[life_expectancy$region ==
                                                    "North America"])

#This could've all been done in one command but this is clearer to read
highest_changed_region <- life_expectancy$region[max(east_asia_mean,
                                                     middle_east_mean,
                                                     europe_mean,
                                                     sub_saharan_africa_mean,
                                                     carribean_mean,
                                                     south_asia_mean,
                                                     north_america_mean)]


# Create a *well labeled* plot (readable title, x-axis, y-axis) showing
# Life expectancy in 1960 v.s. Change in life expectancy
# Programmatically save (i.e., with code, not using the Export button)
# your graph as a .png file in your repo
# Then, in a comment below, *provide an interpretation* of the relationship
# you observe. Feel free to use any library of your choice, or base R functions.
png("plot.png")
plot(x = life_expectancy$le_1960, y = life_expectancy$change,
     xlab = "Life Expectancy in 1960", ylab = "Change in Life Expectancy")
dev.off()
# There seems to be a correlation between a larger life expectancy and a smaller change
# This indicates that the larger life expectancy countries faced smaller change on average
