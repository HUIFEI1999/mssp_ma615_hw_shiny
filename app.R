library(readr)
library("ggplot2")
library(shiny)
library(shinythemes)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

crime <- read_csv("crime incident reports 2022.csv")

theme_set(theme_bw())

geo <- data.frame("y" = crime$Lat,"x" = crime$Long, "i" = crime$DISTRICT)
row_sub = apply(geo, 1, function(row) all(row !=0 ))
geo <- geo[row_sub,]

world <- ne_countries(scale = "medium", returnclass = "sf")

ui <- 
  navbarPage("GROUP 5", collapsible = TRUE, inverse = TRUE, theme = shinytheme("slate"),
             tabPanel("Maps",
                      plotOutput("plot")
             ),
             tabPanel("Table without Location",
                      dataTableOutput("dynamic")
             ), 
             tabPanel("Table with Location",
                      selectInput("dt",
                                  label = "DISTRICT Option",
                                  choices = crime$DISTRICT,
                                  selected = "A1"
                      ),
                      dataTableOutput("dynamic2"),
                      plotOutput("plot2")
                      
             )
             
  )

server <- function(input, output) {
  
  output$dynamic <- renderDataTable(crime, options = list(pageLength = 5))
  
  output$dynamic2 <- renderDataTable(crime[crime$DISTRICT == input$dt,], options = list(pageLength = 5))
  output$plot <- renderPlot({
    
    ggplot(data = world) +
      geom_sf() +
      coord_sf(xlim = c(-70.8, -71.6), ylim = c(41.8, 42.6), expand = FALSE)+
      geom_point(data = geo, aes(x = x, y = y),size = 1,alpha = 0.5,
                 color = "red") 
    
  })
  output$plot2 <- renderPlot({
    ggplot(data = world) +
      geom_sf() +
      coord_sf(xlim = c(-70.8, -71.6), ylim = c(41.8, 42.6), expand = FALSE)+
      geom_point(data = geo[geo$i == input$dt,], aes(x = x, y = y),size = 1,alpha = 0.5,
                 color = "red") 
    
  })
  
}
shinyApp(ui = ui, server = server)
