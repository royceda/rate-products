library(shiny)


# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Cours Finance: TP2"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("cap","capital :",40000),
    numericInput("periode","periode :", 1),
    numericInput("fraisfixes","frais fixes :", 120),
    numericInput("CRD","CRD :", 100),
    numericInput("flux d'assurance","flux d'assurance :", 12),
    numericInput("taux","taux :", 0.01),
    

    sliderInput("taux1",
                "Taux 1:",
                min = 0.001,
                max = 1,
                value = 0.02,
                step= 0.001),
    sliderInput("taux2",
                "Taux 2:",
                min = 0.001,
                max = 1,
                value = 0.02,
                step= 0.001),
    sliderInput("taux3",
              "Taux 3:",
              min = 0.001,
              max = 1,
              value = 0.02,
              step= 0.001),
    sliderInput("taux4",
            "Taux 4:",
            min = 0.001,
            max = 1,
            value = 0.02,
            step= 0.001),
  sliderInput("taux5",
            "Taux 5 :",
            min = 0.001,
            max = 1,
            value = 0.02,
            step= 0.001)
  ),
  
  

  # Show a plot of the generated distribution
  mainPanel(
    #    textOutput("vie"),
    tableOutput("vie1"),
    plotOutput("vie2")
    #   plotOutput("distPlot")
  )
))
