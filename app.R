#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

ui <- 
  navbarPage("Demo", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("Participation"),
             tabPanel("Service Use",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Accessing Website"),
                          tabPanel("Visiting Library"),
                          tabPanel("Attending Workshops"),
                          tabPanel("Exploring Technology")
                        ))),
             tabPanel("Space & Study Habits",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Study Habit"),
                          tabPanel("Space Preference - Mid & Final Terms"),
                          tabPanel("Space Preference - Most Days"),
                          tabPanel("Space Preference - Student Submissions")
                        ))), 
             tabPanel("Outreach"),
             tabPanel("About")
  )

server <- function(input, output) {}

shinyApp(ui = ui, server = server)