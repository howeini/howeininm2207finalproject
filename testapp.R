library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

# Define UI ----
ui <- fluidPage(
  titlePanel("Song Attributes"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("sel_attributes", 
                         label = "Select the attributes to be displayed",
                         choices = c("danceability", "energy", "speechiness", "acousticness", 
                                     "instrumentalness", "liveness", "valence"),
                         selected = c("danceability")),
      
      sliderInput("bins", 
                  label = "Number of Bins:",
                  min = 1, max = 50, value = 25)
    ),
    
    mainPanel(
      plotOutput("hist")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$hist <- renderPlot({
    raw <- read_csv("Jan2022_Spotify_Weekly_Top_200_Songs_Streaming_Data_by_country_view_view.csv")
    spotify <- subset(raw, select = -c(X_1, artists_num, artist_genre, collab, album_num_tracks, pivot))
    track_attribute1 <- spotify %>%
      select(track_name, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) %>%
      distinct(track_name, .keep_all = TRUE) %>%
      pivot_longer(cols= danceability:valence,
                   names_to = "attribute",
                   values_to = "value") %>%
      filter(attribute %in% as.vector(input$sel_attributes))
    
    
    ggplot(data = track_attribute1, 
           mapping = aes(x = value, fill=attribute)) + 
      geom_histogram(alpha=0.5, bins = input$bins) +
      labs(y = "Frequency", title = "Frequency of Attribute")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)