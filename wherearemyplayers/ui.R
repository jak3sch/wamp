#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(
      title = "Who Are My Players?",
      titleWidth = "20%",
      disable = TRUE
    ),

    # Sidebar with a slider input for number of bins
    shinydashboard::dashboardSidebar(
      selectizeInput(
        "platform",
        "Select platform",
        choices = c("MFL", "Sleeper", "Fleaflicker"),
        options = list(
          placeholder = "Please choose one",
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      uiOutput("inputs"),
      uiOutput("userLeagues"),
      width = "20%"
    ),

    # Show a plot of the generated distribution
    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      titlePanel(
        tags$h1(
          tags$span("Who"),
          tags$span("Are"),
          tags$span("My"),
          tags$span("Players"),
          paste(" in the week", current_week, "Matchups?")
        ),
        windowTitle = paste("WAMP week ", current_week)
      ),
      uiOutput("dynamic_ui"),
      width = "80%"
    )
)
