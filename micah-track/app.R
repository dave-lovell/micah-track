library(shiny)
library(dplyr)
library(googlesheets4)
library(carutools)

# Define UI for application that draws a histogram
true_pin <- readLines("secrets/pin")

ui <- fluidPage(
  titlePanel("Stats from the last 24 hours"),

  passwordInput("pin", label = "Enter pin:"),

  conditionalPanel( #Pin bad
    input.pin != true_pin,
    p("")
  ),

  conditionalPanel( #Pin good
    input.pin == true_pin,
    p("Welcome!")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
