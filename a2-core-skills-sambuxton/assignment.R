# assignment 2-foundational-skills


# Set up and Defining variables ------------------------------------------------

# Install and load the the `stringr` package
# It has a variety of functions that make working with string variables easier
install.packages("stringr")
library(stringr)

# Create a numeric variable `my_age` that is equal to your age
my_age <- 20

# Create a variable `my_name` that is equal to your first name
my_name <- "Sam"

# Using multiplication, create a variable `minutes_in_a_day` that is
# equal to the number of minutes in a day
minutes_in_a_day <- 60 * 24

# Using multiplication, create a variable `hours_in_a_year` that is
# equal to the number of hours in a year
hours_in_a_year <- 24 * 365


# Create a variable `more_minutes_than_hours` that is boolean (TRUE/FALSE)
# It should be TRUE if there are more minutes/day than hours/year
# Otherwise it should be FALSE
more_minutes_than_hours <- (minutes_in_a_day > hours_in_a_year)

# Working with functions -------------------------------------------------------

# Write a function `make_introduction()` that takes in two args: name, and age.
# This function should return a string value that says
# "Hello, my name is {name}, and I'm {age} years old.".
make_introduction <- function(name, age) {
  paste("Hello, my name is", name, "and I'm ", age, "years old.")
}
make_introduction("Sam", 20)

# Create a variable `my_intro` by passing your variables `my_name` and `my_age`
# into your `make_introduction()` function
my_intro <- make_introduction(my_name, my_age)

# Create a variable `casual_intro` by substituting "Hello, my name is ",
# with "Hey, I'm" in your `my_intro` variable
casual_intro <- str_replace(my_intro, "Hello", "Hey")

# Create a variable `loud_intro`, which is `my_intro` in all upper-case letters
loud_intro <- str_to_upper(my_intro)

# Create a variable `quiet_intro`, which is `my_intro` in all lower-case letters
quiet_intro <- str_to_lower(my_intro)

# Create a new variable `capitalized`, which is your `my_intro` variable, but
# with each word capitalized. hint: use the stringr function `str_to_title`
capitalized <- str_to_title(my_intro)

# Using the `str_count` function, create a variable `occurrences` that stores
# the # of times the letter "e" appears in `my_intro`
occurences <- str_count(my_intro, "e")

# Write a function `double()` that takes in a value and
# returns that value times two
double <- function(value) {
  value * 2
}


# Using your `double()` function, create a variable `minutes_in_two_days`,
# which is the number of minutes in two days
minutes_in_two_days <- double(minutes_in_a_day)


# Write a function `cube()` that takes in a value and returns that value cubed
cube <- function(value) {
  value * value * value
}

# Create a variable `twenty_seven` by passing 3 to your `cube()` function
twenty_seven <- cube(3)

# Vectors ----------------------------------------------------------------------

# Create a vector `movies` that contains the names of six movies you like
movies <- c("Interstellar", "Let Me In", "Eternal Sunshine",
            "Star Trek: Beyond", "Arrival", "District 9")


# Create a vector `top_three` that only contains the first three movies
# You should do this by subsetting the vector, not by simply retyping the movies
top_three <- movies[1:3]

# Using your vector and the `paste()` method, create a vector `excited` that
# adds the phrase - " is a great movie!" to the end of each element `movies`
# Added space to end for clarity
excited <- paste(movies, collapse = " is a great movie!")

# Create a vector `without_four` by omitting the fourth element from `movies`
# Again, do this by subsetting the vector, not by simply retyping the movies
without_four <- movies[-4]


# Create a vector `numbers` that is the numbers 700 through 999
numbers <- seq(from = 700, to = 999)

# Using the built in `length()` function, create a variable `len` that is
# equal to the length of your vector `numbers`
len <- length(numbers)

# Using the `mean()` function, create a variable `numbers_mean` that is
# equal to the mean of your vector `numbers`
numbers_mean <- mean(numbers)

# Using the `median()` function, create a variable `numbers_median`
# that is the median of your vector `numbers`
numbers_median <- median(numbers)

# Create a vector `lower_numbers` that is the numbers 500:699
lower_numbers <- seq(from = 500, to = 699)

# Create a vector `all_numbers` that combines `lower_numbers` and `numbers`
all_numbers <- c(lower_numbers, numbers)

# Dates ------------------------------------------------------------------------

# Use the appropriate function to create a variable `today` storing today's date
# Make sure to use R to get the *current date*
# See https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.time.html
# Had to change timezone because computer date isn't compatible with R
Sys.setenv(TZ = "US/PAcific")
# I could also use asDate 
today <- as.Date(Sys.Date(), "%d%b%Y")


# Create a variable `summer_break` that represents the first day of summer break
# (June 8, 2018). Make sure to use the `as.Date` function again
summer_break <- as.Date("8jun2018", "%d%b%Y")

# Create a variable `days_to_break` that is how many days until break
# Hint: subtract the dates!
days_to_break <- summer_break - today
days_to_break
# Define a function `make_birthday_intro()` that takes in three arguments:
# a name, an age, and a character string for your next (upcoming) birthday.
# This method should return a character string of the format:
# "Hello, my name is {name}, and I'm {age} years old.
#  In {N} days I'll be {new_age}."
# You must utilize your `make_introduction()` function from Part 1,
# and compute {N} and {new_age} in your function
make_birthday_intro <- function(name, age, birthday) {
  new_age <- age + 1
  birthday_date <- as.Date(birthday, "%d%b%Y")
  paste(make_introduction(name, age), "In", birthday_date - today,
        "days I'll be", new_age)
}

# Create a variable `my_bday_intro` using the `make_birthday_intro()` function,
# passing in `my_name`, `my_age`, and your upcoming birthday.
my_bday_intro <- make_birthday_intro("Sam", 20, "22mar2019")

# Challenge ------------------------------------------------------------------
# Write a function `remove_digits` that will remove all digits
# (i.e., 0 through 9) from all elements in a *vector of strings*.
remove_digits <- function(sentence) {
  gsub("[0-9]+", "", sentence)
}

# Demonstrate that your approach is successful by passing a vector of courses
# to your function. For example, remove_digits(c("INFO 201", "CSE 142"))
remove_digits(c("INFO 201", "CSE 142"))