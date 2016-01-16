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
  headerPanel("Cours Finance"),
  sidebarLayout(
    # Sidebar with a slider input for number of observations
    column(width = 12,
    textInput("flp"," flux passif :", "1000 5000  8000  12000  500  7000  30000 20000 40000  10000 30000  25000 10000 10000 10000 10000 10000 10000 10000 10000" ),
    tags$style(type='text/css', "#flp { width: 1050px; }"),
    textInput("dp","  dates passif :","1      2      3     4    5     6     7     8     9    10    11      12    13    14     15   16     17   18    19   20 "),
    tags$style(type='text/css', "#dp { width: 1050px; }"),
    textInput("cbtp"," taux actif :",    "0.005 0.006 0.007 0.01 0.012 0.013 0.015 0.016 0.019 0.021  0.023 0.023 0.025 0.026 0.027 0.028 0.028 0.028 0.03 0.031"),
    tags$style(type='text/css', "#cbtp { width: 1050px; }"),
    textInput("matp"," maturité actif :","1.      2.      3.     4.    5.     6.     7.     8.     9.    10.    11.      12.    13.   14.     15.   16.     17.   18.    19.   20. "),
    tags$style(type='text/css', "#matp { width: 1050px; }"),
    sliderInput("vtaux",
                "variation taux :",
                min = -0.02,
                max = 0.02,
                value = 0.003,
                step= 0.001),
     sliderInput("taux",
                "Taux actuariel :",
                min = 0.001,
                max = 1,
                value = 0.02,
                step= 0.001)
  ),
  column(
    4,
     tableOutput("vie")
 #   plotOutput("distPlot")
  )
)))
