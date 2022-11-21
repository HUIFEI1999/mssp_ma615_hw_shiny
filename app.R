library(readr)
library(shiny)
library(shinythemes)
library("rnaturalearth")
library("rnaturalearthdata")
library(geojsonio)
library(broom)
library(sf)
library(osmdata)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(mapproj)

crime <- read_csv("crime incident reports 2022.csv")


spdf_file_bos <- geojson_read(  # Read the geojson file
  "Boston_Neighborhoods.geojson",
  what = "sp"
)
stats_df <- as.data.frame(spdf_file_bos)  # Export the census statistics in another data frame variable\

spdf_file_bos <- tidy(  # Convert it to a spatial data frame, with zip code as index
  spdf_file_bos,
  region="Neighborhood_ID"  # Use ZIPCODE variable as index, the index will be named "id"
)

crime <- crime %>%  # Replace missing inspection grades with NA
  mutate(OFFENSE_DESCRIPTION=replace(OFFENSE_DESCRIPTION, OFFENSE_DESCRIPTION == "", NA))
crime <- crime %>% filter(Long != 0 & Lat != 0)

simple_crime <- crime[1:100,]

sick_ass <- crime %>% filter(OFFENSE_DESCRIPTION == "SICK ASSIST")


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
    
    ggplot() +
      geom_polygon(data=spdf_file_bos,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.2) +
      
      geom_point(data=sick_ass,
                 aes(x=Long,
                     y=Lat),
                 color="red",
                 alpha=.6,
                 size=0.5,
                 shape=20) +
      coord_map() +
      labs(title="CRIME in Boston City")
    
  })
  output$plot2 <- renderPlot({
    ggplot() +
      geom_polygon(data=spdf_file_bos,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.2) +
      
      geom_point(data=sick_ass[sick_ass$DISTRICT == input$dt,],
                 aes(x=Long,
                     y=Lat),
                 color="red",
                 alpha=.6,
                 size=0.5,
                 shape=20) +
      coord_map() +
      labs(title="CRIME in Boston City")
    
  })
  
}
shinyApp(ui = ui, server = server)
