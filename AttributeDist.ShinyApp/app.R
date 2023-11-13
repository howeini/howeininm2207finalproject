library(shiny)
library(tidyverse)
library(ggplot2)

# Attribute descriptions data frame
attribute_descriptions <- data.frame(
  attribute = c("danceability", "energy", "speechiness", "acousticness", 
                "instrumentalness", "liveness", "valence"),
  description = c(
    "How suitable a track is for dancing based on a combination of musical elements 
    including tempo, rhythm stability, beat strength, and overall regularity.",
    
    "Represents a perceptual measure of intensity and activity. 
    Typically, energetic tracks feel fast, loud, and noisy. 
    For example, death metal has high energy, while a Bach prelude scores low on the scale. 
    Perceptual features contributing to this attribute include dynamic range, perceived loudness, 
    timbre, onset rate, and general entropy.",
    
    "Speechiness detects the presence of spoken words in a track. 
    The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), 
    the closer to 1.0 the attribute value. 
    Values above 0.66 describe tracks that are probably made entirely of spoken words. 
    Values between 0.33 and 0.66 describe tracks that may contain both music and speech,
    either in sections or layered, including such cases as rap music. 
    Values below 0.33 most likely represent music and other non-speech-like tracks.",
    
    "A confidence measure of whether the track is acoustic. 
    1.0 represents high confidence the track is acoustic.",
    
    "Predicts whether a track contains no vocals. 
    'Ooh' and 'aah' sounds are treated as instrumental in this context. 
    Rap or spoken word tracks are clearly 'vocal'. 
    The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. 
    Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.",
    
    "Detects the presence of an audience in the recording. 
    Higher liveness values represent an increased probability that the track was performed live. 
    A value above 0.8 provides strong likelihood that the track is live.",
    
    "Describes the musical positiveness conveyed by a track. 
    Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
    while tracks with low valence sound more negative (e.g. sad, depressed, angry)"
  )
)

# Define UI ----
ui <- fluidPage(
  h1("Select the attributes to be displayed"),
  
  fluidRow(
    
    column(width = 2,
           hr(),
           checkboxGroupInput("sel_attributes", 
                              label = h4("Song Attributes:"),
                              choices = c("danceability", "energy", "speechiness", "acousticness", 
                                          "instrumentalness", "liveness", "valence"),
                              selected = c("danceability")
                              )),
    column(width = 5,
           hr(),
           h3("Explore the data at different levels of detail"),
           p("A", span("smaller", style = "color:red; font-size:14px; text-decoration: underline"), 
           "number of bins can provide an overview of the general shape of the distribution, 
             while a", span("larger", style = "color:red; font-size:14px; text-decoration: underline"), 
           "number of bins can reveal smaller peaks, valleys, or outliers."),
           sliderInput("bins", 
                       label = h4("Number of Bins:"),
                       min = 1, max = 50, value = 25)
           ),
           
    column(width = 5,
           hr(),
           h3("Unsure on how to interpret a histogram?"),
           checkboxInput("show_interpretation",
                         label = ("Show how to interpret a histogram"),
                         value = FALSE),
           htmlOutput("interpretation")
           )
    ),
  
  hr(),
  
  fluidRow(
    column(width = 7,
           plotOutput("hist")),
    
    column(width = 5,
           h4("Here is a description of your selected attributes:"),
           p("Each attribute's value ranges from 0.0 to 1.0, with 0.0 indicating the minimum and 1.0 indicating the maximum value."),
           p("For example, for the danceability attribute, 0.0 means least danceable, and 1.0 means most danceable."),
           br(),
           tableOutput("description"))
    ))



# Define server logic ----
server <- function(input, output) {
  output$hist <- renderPlot({
    raw <- read_csv("Jan2022_Spotify_Weekly_Top_200_Songs_Streaming_Data_by_country_view_view.csv")
    spotify <- raw %>% select(-X_1, -artists_num, -artist_genre, -collab, -album_num_tracks, -pivot)
    
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
      labs(x = "Value", y = "Frequency", title = "Frequency of Attribute")
  }, height = 600)
  
  output$description <- renderTable({
    selected_attributes <- input$sel_attributes
    descriptions <- attribute_descriptions[attribute_descriptions$attribute %in% selected_attributes, ]
    descriptions
  },
  striped = FALSE,  # Add striped rows
  hover = TRUE,    # Highlight rows on hover
  bordered = TRUE,  # Add borders to the table
  spacing = "m",   # Set spacing between cells (options: "s", "xs", "m", "l")
  width = "100%",  # Set table width
  align = "c",     # Align cell content (options: "l" - left, "c" - center, "r" - right)
  rownames = FALSE, # Show/hide row names
  colnames = TRUE,  # Show/hide column names
  digits = 2,       # Number of decimal places for numeric columns
  na = "",          # String to display for missing values
  quoted = FALSE    # Quote character for column names if TRUE
  )
  
  output$interpretation <- renderText({
    req(input$show_interpretation)
    if (input$show_interpretation) {
      text <- "
              <ul> <li> Each bar shows the <b> frequency </b> of data points falling within a specific range </li>
              <li> This gives a quick glance at the dataset's <b> most common values </b> </li>
              <li> <b> Bar Height: </b> Indicates popularity
                    <ul> <li> Taller bars = higher frequency </li> </ul>
                    </li> 
              <li> <b> Histogram Width: </b> Reflects range of values
                    <ul> <li> Wider histograms = broad dataset </li> </ul>
                    </li>
              <li> <b> Outliers </b> stand out as individual points detached from the main cluster of bars </li>"
      HTML(text)
    } else {
      NULL
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)