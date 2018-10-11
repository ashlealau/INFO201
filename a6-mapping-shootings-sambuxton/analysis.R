library(plotly)
library(dplyr)
library(ggplot2)

shootings <- data.frame(read.csv(
                          "./data/shootings-2018.csv",
                          stringsAsFactors = FALSE)
                        )
# How many shootings occurred? 
# How many lives were lost?
# Which cities that were most impacted (you can decide how to measure "impact")?
# At least one other insight of your choice.

num_shootings <- nrow(shootings)
num_fatalities <- sum(shootings$num_killed)

# For impact I decided to measure top 3 cities with most injuries
impact_cities <- arrange(shootings, -num_killed) %>%
                   select(city)

top_impacted <- impact_cities[1:3, 1]
impact_statement <- paste0(
                      top_impacted[1], ", ",
                      top_impacted[2], ", and ",
                      top_impacted[3]
                    )

# For my insight I chose total fatality rate
total_fatality_rate <- round(
                         (num_fatalities / (sum(shootings$num_injured)
                          + num_fatalities)) * 100, 2
                       )

# For the incident I am choosing the deadliest
deadliest_attacks <- filter(
                      shootings,
                      num_killed / ( num_killed + num_injured) ==
                      max(num_killed / ( num_killed + num_injured))
                    )

# Randomly picks out first deadliest attack if tied
deadliest_attack <- mutate(
                      deadliest_attacks[1, ],
                      fatality_rate = num_killed /
                                        (num_killed + num_injured) * 100)

# Summary table
state_summary <- group_by(shootings, state) %>%
  summarize(
    fatality_rate = sum(num_killed) / sum(num_killed + num_injured),
    total_killed = sum(num_killed),
    total_injured = sum(num_injured),
    num_shootings = length(city)
  ) %>%
  arrange(-num_shootings)
# Interestingly, Florida is the only state required to publish all police records
# which explains why it has the most recorded shootings. There's a correlation between
# Fatality rate and number of shootings, and between latitude (location) and number of shootings
# and the fatality of the shootings with Southern states more likely to have a higher fatality rate



shooting_data <- select(
                   shootings, state,
                   city, lat, lng,
                   num_killed, num_injured
                 )

# My map
library(plotly)

us <- map_data("state")
library(maps)
# If the hoverinfo function worked (it does not, none of the examples on the plotly website work for some reason)
# I would have added the command plot <- ggplotly(g) and added the hoverinfo information. 
map_plot <- ggplot() +
  geom_polygon(
    data = us, aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.3) +
  geom_point(
    data = shootings,
    aes(
      x = lng, y = lat, size = num_killed,
      color = num_injured)
    ) +
  labs(
    color = "Number Injured", size = "Number Killed",
    x = "Longitude", y = "Latitude"
  )

# My plot demonstrating lack of variance in shootings to demonstrate the link between state policy
# and the fatality rate of the shooting
incident_deadliness <- mutate(
                         shootings,
                         fatality_rate = (num_killed /
                                          (num_killed + num_injured)
                                         )
                       )
severity_plot <- ggplot(
                   incident_deadliness,
                   aes(x = state, y = fatality_rate, fill = state)
                 ) +
                 geom_boxplot() +
                 labs(x = "States", y = "Fatality Rate", fill = "State") +
                 theme(
                   axis.text.x = element_text(
                                   angle = -90,
                                   vjust = 0.4,
                                   hjust = 1
                                 ),
                   axis.text.y = element_text(
                                   angle = 90,
                                   vjust = 0.4,
                                   hjust = 1
                                 )
                 )
       
severity_plot <- ggplotly(severity_plot)
