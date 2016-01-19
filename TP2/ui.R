library(shiny)


# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Cours Finance: TP2"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("cap","capital :",100000),
    numericInput("debut","Debut :",1),
    numericInput("periode","periode :", 2),
    numericInput("taux","taux :", 0.01),
    

    sliderInput("taux1",
                "taux1",
                min = 0.001,
                max = 0.5,
                value = 0.01,
                step= 0.001),
    sliderInput("taux2",
                "Taux 2:",
                min = 0.001,
                max = 0.5,
                value = 0.02,
                step= 0.001),
    sliderInput("taux3",
              "Taux 3:",
              min = 0.001,
              max = 0.5,
              value = 0.02,
              step= 0.001)
  ),
  
  

  # Show a plot of the generated distribution
  mainPanel(
    #textOutput("vie"),
    tableOutput("vie1"),
    #plotOutput("vie2")
    #   plotOutput("distPlot")
  )
))
