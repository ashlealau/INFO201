# Didn't include rsconnect code, only used to publish on my end!
library(shiny)
library(rsconnect)
library(ggplot2)

my_ui <- fluidPage(theme = "bootstrap.css",
  mainPanel("Welcome to my Midwest Graphics Page"),
  navbarPage("The Midwest in Graphics",
             tabPanel(
               "Histogram",
               sidebarPanel(width = 3,
                            div(id = "options", radioButtons(
                              inputId = "statistic",
                              label = "Tell me about the...",
                              choices = c("Black population",
                                          "Asian population",
                                          "College Education Rate",
                                          "Poverty Rate")
                                          )
                            ),
                            sliderInput(
                              inputId = "cases",
                              label = "Filter by only the top
                                       (number) largest counties",
                              min = 1, max = nrow(midwest),
                              value = 400)
             ),
             mainPanel(plotOutput("histogram"))
             ),
             tabPanel(
                     "Plot vs. Size",
                     sidebarPanel(
                       radioButtons(
                                    inputId = "compare",
                                    label = "Compare the size of
                                             each county to its...",
                                    choices = c("Black population",
                                                "Asian population",
                                                "College Education Rate",
                                                "Poverty Rate")
                                   ),
                       selectInput(
                                   inputId = "color_scheme",
                                   label = "Which color scheme would you like?",
                                   choices = c("Red", "Green", "Blue")
                                  )
                     ),
                     mainPanel(plotOutput("plot"))
             ),
             tabPanel(
               "Summary",
               p("The Midwest covers approximately",
                 sum(midwest$area), "square miles and",
                 "has a total population of", sum(midwest$poptotal)),
               p("Additionally, it is made up of", nrow(midwest),
                 "counties and is demographically",
                 round(sum(midwest$popblack) / sum(midwest$poptotal) * 100, 2),
                 "percent black",
                 round(sum(midwest$popwhite) / sum(midwest$poptotal) * 100, 2),
                 "percent white, and",
                 round(sum(midwest$popasian) / sum(midwest$poptotal) * 100, 2),
                 "percent asian."),
               p("Lastly, the counties of Midwest have an
                  average college education rate of",
                 round(mean(midwest$percollege, 1)),
                 "percent and an overall poverty rate of",
                 round(sum(midwest$poppovertyknown) /
                           sum(midwest$poptotal) * 100, 1),
                 "percent."
                 ),
               p("Here is an image of the Midwest:
                 (only available on some browsers)"
               ),
               img(
                   src = "midwest.png"
               )
             ),
             tabPanel("Raw Data", tableOutput("table"))
  )
)