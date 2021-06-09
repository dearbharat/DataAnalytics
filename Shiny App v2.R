# Load Libraries
library(tidyverse)
library(shiny)
library(tidytext)
library(plotly)
library(glue)

# ui User Interface
ui <- fluidPage(
  titlePanel("Scotland and UK COVID-19 Speeches"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "origin",
        label   = "Select origin",
        choices = c("Scotland", "UK")
      )),
    mainPanel(
      plotOutput(outputId = "sentiment_plot")
    )
  )
)

# Server
server <- function(input, output) {
  output$sentiment_plot <-  renderPlot({
    csw <- covid_speeches_words %>%
      filter(origin == input$origin) %>%
      anti_join(stop_words) %>%
      filter(word != "positive") %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = date, y = sentiment)) +
      geom_line(color = "gray") +
      geom_point(aes(color = sentiment > 0), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      labs(title = glue("Daily sentiment score, {input$origin} COVID-19 briefings"),
           x = "Date", y = "Sentiment score (positive - negative)") +
      theme_minimal() + theme(legend.position = "none")
    
  })
}

# Shiny App
shinyApp()




  