library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Summary Table"),
  sidebarLayout(
    sidebarPanel(
      selectInput("y", "Select Variable", choices = colnames(iris)),
    ),
    mainPanel(
      tableOutput("demo_table")
    )
  )
)

server <- function(input, output, session) {
  output$demo_table <- renderTable({
    raw <- readr::read_csv("/Users/howeini/howeini/Jan2022_Spotify_Weekly_Top_200_Songs_Streaming_Data_by_country_view_view.csv")
    spotify <- subset(raw, select = -c(X_1, artists_num, artist_genre, collab, album_num_tracks, pivot))
  })
}

shinyApp(ui, server)
