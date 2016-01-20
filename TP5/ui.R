library(shiny)
# Define UI for application that plots random distributions
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      h1 {
                    font-family: 'King Will Be King',regular;
                    font-weight: 500;
                    line-height: 1.2;
                    color: #18da33;
                    }
                    "))
    ),
  # Application title
  headerPanel("Interpolation"),
  sidebarLayout(
    # Sidebar with a slider input for number of observations
    column(width = 12,
      textInput("x"," maturité actif :","1.      2.      3.     4.    5.     6.     7.     8.     9.    10.    11.      12.    13.   14.     15.   16.     17.   18.    19.   20. "),
      tags$style(type='text/css', "#x { width: 1050px; }"),
      textInput("y"," taux actif :",    "0.005 0.006 0.007 0.01 0.012 0.013 0.015 0.016 0.019 0.021  0.023 0.023 0.025 0.026 0.027 0.028 0.028 0.028 0.03 0.031"),
      tags$style(type='text/css', "#y { width: 1050px; }"),

      column(
        4,
        textOutput("vie"),
        plotOutput("vie1")
        )
        )))
