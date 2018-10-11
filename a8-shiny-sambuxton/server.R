library(shiny)
library(dplyr)
library(ggplot2)

my_server <- function(input, output) {
  filtered_midwest <- arrange(midwest, -poptotal)
  stat_translator <- as.list(setNames(c("percblack", "percasian",
                                        "percollege", "percbelowpoverty"
                                       ),
                                      c("Black population",
                                        "Asian population",
                                        "College Education Rate",
                                        "Poverty Rate")
                                       )
                                     )
  output$table <- renderTable({
    midwest
  })
  sized_data <- reactive({
    return(head(filtered_midwest, input$cases))
  })
  output$histogram <- renderPlot({
    comparison <- stat_translator[[input$statistic]]
    histogram <- hist(sized_data()[[comparison]],
                      xlab = "Percentage of People with this Characteristic",
                      ylab = "Number of Counties",
                      main = "Histogram of Characteristic")
    return(histogram)
  })
  output$plot <- renderPlot({
    # This line may seem repetitive but I wrote it this way so
    # that the comparison choicesfor the chart/histogram
    # can be changed individually
    comparison <- stat_translator[[input$compare]]
    ggplot(sized_data(),
           aes(x = sized_data()[[comparison]],
               y = sized_data()$poptotal)) +
      geom_point(shape = 18, color = input$color_scheme, size = 3) +
      geom_smooth(
        method = lm,  linetype = "dashed",
        color = "darkred",
        fill = paste0("dark", input$color_scheme)) +
      xlab("Percentage of People with This Characteristic") +
      ylab("Size of County /(Population/)")
  })
}