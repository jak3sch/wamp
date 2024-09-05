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
shinydashboardPlus::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = "Who Are My Players?",
      titleWidth = "20%",
      disable = TRUE
    ),

    # Sidebar with a slider input for number of bins
    sidebar = shinydashboard::dashboardSidebar(
      selectizeInput(
        "platform",
        "Select platform",
        choices = c("Fleaflicker", "MFL", "Sleeper"),
        options = list(
          placeholder = "Please choose one",
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),

      uiOutput("inputs"),
      uiOutput("sidebarErrors"),
      uiOutput("userLeagues"),
      width = "20%"
    ),

    # Show a plot of the generated distribution
    body = shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      uiOutput("window_title"),
      uiOutput("dynamic_ui"),
      width = "80%"
    ),

    footer = shinydashboardPlus::dashboardFooter(right = "@Jakob Eschler / nflverse"),
    skin = "midnight"
)
