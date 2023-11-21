library(shiny)
library(tidyverse)
library(ggplot2)
library(markdown)
library(plotly)

# Attribute descriptions data frame
attribute_descriptions <- data.frame(
  attribute = c("danceability", "energy", "speechiness", "acousticness", 
                "instrumentalness", "liveness", "valence"),
  description = c(
    "Centered around a value of 0.75, majority of songs boast a considerable degree of danceability.",
    
    "Although the majority of songs exhibit moderate to high energy levels,
     the overall energy range is wide.",
    
    "Most songs are music and non-speech-like tracks.",
    
    "Generally, there is low confidence that songs are acoustic.",
    
    "Aside from a few anomalies, the songs contain vocals.",
    
    "Aside from a few anomalies, the songs were not performed live.",
    
    "While there are more songs that sound positive than negative, the distribution is relatively more balanced. "
  )
)



# Define UI ----
ui <- navbarPage("Song Attributes",
                 tabPanel("Numerical Atrributes",
                          fluidPage(
                            
                            fluidRow(
                                     h3("Unsure on how to interpret a histogram?"),
                                     checkboxInput("show_interpretation",
                                                   label = ("Show how to interpret a histogram"),
                                                   value = FALSE),
                                     htmlOutput("interpretation")),
                            hr(),
                            
                            h3("Attributes with values ranging from 0.0-1.0"),
                            
                            fluidRow(
                              column(width = 9,
                                     plotOutput("hist")),
                              
                              column(width = 3,
                                fluidRow(
                                       checkboxGroupInput("sel_attributes", 
                                                          label = h4("Song Attributes:"),
                                                          choices = c("danceability", "energy", "speechiness", "acousticness", 
                                                                      "instrumentalness", "liveness", "valence"),
                                                          selected = c("danceability")
                                       )),
                                
                                fluidRow(
                                       sliderInput("bins", 
                                                   label = h4("Number of Bins:"),
                                                   min = 1, max = 50, value = 25))
                              )),
                            
                            fluidRow(
                                   h4("Here are the insights from your selected attributes:"),
                                   tableOutput("description")),
                            
                            
                            
                            
                            hr(),
                            fluidRow(h3("Loudness"),
                                     column(width = 9,
                                            plotlyOutput("loud_hist")),
                                     
                                     column(width = 3,
                                            sliderInput("bins_loud", 
                                                        label = h4("Number of Bins:"),
                                                        min = 1, max = 50, value = 25))),
                            br(),
                            fluidRow(htmlOutput("loud_desc")),
                            
                            hr(),
                            fluidRow(h3("Tempo"),
                                     column(width = 9,
                                            plotlyOutput("tempo_hist")),
                                     
                                     column(width = 3,
                                            sliderInput("bins_tempo", 
                                                        label = h4("Number of Bins:"),
                                                        min = 1, max = 50, value = 25))),
                            br(),
                            fluidRow(htmlOutput("tempo_desc")),
                            
                            hr(),
                            fluidRow(h3("Duration"),
                                     column(width = 9,
                                            plotlyOutput("dur_hist")),
                                     
                                     column(width = 3,
                                            sliderInput("bins_dur", 
                                                        label = h4("Number of Bins:"),
                                                        min = 1, max = 50, value = 25))),
                            br(),
                            fluidRow(htmlOutput("dur_desc"))
                            )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Categorical Attributes",
                          fluidPage(

                              column(width = 6,
                                     
                                     h3("Key"),
                                     p("C♯/D♭ is the most popular key, followed by C then G."),
                                     plotlyOutput("key_bar")),
                              
                              column(width = 6,
                                     
                                     h3("Mode"),
                                     p("While there are more songs in Major scale than Minor scale, the distribution is relatively balanced."),
                                     plotlyOutput("mode_pie")))
                            
                           
                                    )
                                  )
                                

# Define server logic ----
raw <- read_csv("Jan2022_Spotify_Weekly_Top_200_Songs_Streaming_Data_by_country_view_view.csv")
spotify <- raw %>% select(-X_1, -artists_num, -artist_genre, -collab, -album_num_tracks, -pivot)

server <- function(input, output) {
  output$hist <- renderPlot({
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
  }, height = 400)
  
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
    <div style='display: flex;'>
        <div style='flex: 1; margin-right: 10rem;'>
            <ul> 
                <li> Each bar shows the <b> frequency </b> of data points falling within a specific range </li>
                <li> This gives a quick glance at the dataset's <b> most common values </b> </li>
                <li> <b> Bar Height: </b> Indicates popularity
                    <ul> 
                        <li> Taller bars = higher frequency </li> 
                    </ul>
                </li> 
                <li> <b> Histogram Width: </b> Reflects range of values
                    <ul> 
                        <li> Wider histograms = broad dataset </li> 
                    </ul>
                </li>
                <li> <b> Outliers </b> stand out as individual points detached from the main cluster of bars </li>
            </ul>
        </div>
        <div style='flex: 1; background-color: #ebebeb; 
  padding: 2rem;
  border-radius: 0.25rem;'>
            <strong><em style='font-size: larger;'>Explore the data at different levels of detail!</em></strong>
            <br>
            <br>
            A <span style='color: red; text-decoration: underline;'>smaller</span> number of bins can provide an overview of the general shape of the distribution, 
            while a <span style='color: red; text-decoration: underline;'>larger</span> number of bins can reveal smaller peaks, valleys, or outliers.
            <br>
            <br>
            Use the sliders to change the number of bins, and observe how the histograms change!
        </div>
    </div>"
      
      
      HTML(text)
    } else {
      NULL
    }
  })
  
  
  
  
  
  
  track_attribute2 <- spotify %>%
    select(track_name, key, mode) %>%
    distinct(track_name, .keep_all = TRUE)
  
  # Function to map integers to pitch names
  map_to_pitch <- function(key) {
    pitches <- c("C", "C♯/D♭", "D", "D♯/E♭", "E", "F", "F♯/G♭", "G", "G♯/A♭", "A", "A♯/B♭", "B")
    return(pitches[key + 1])
  }
  
  # Function to map integers to pitch names
  map_to_mode <- function(scale) {
    modes <- c("Minor", "Major")
    return(modes[scale + 1])
  }
  
  # Apply the function to create new columns
  track_attribute2$key_alphabet <- sapply(track_attribute2$key, map_to_pitch)
  track_attribute2$mode_word <- sapply(track_attribute2$mode, map_to_mode)
  
  track_attribute2 <- na.omit(track_attribute2)
  
  output$key_bar <- renderPlotly({
    kb <- ggplot(track_attribute2, aes(x = key_alphabet)) + 
      geom_bar() + 
      labs(x= "Key", y="Count", title = "Count of Key") + 
      theme(axis.text.x = element_text(family = "Arial Unicode MS"))
    
    kb <- ggplotly(kb, tooltip = "y")
    
    kb
  })
  
  output$mode_pie <- renderPlotly({
    mb <- track_attribute2 %>%
      count(mode_word) %>%
      plot_ly(labels = ~mode_word, values = ~n, type = 'pie') %>%
      layout(title = "Distribution of Mode", showlegend = TRUE) %>%
      add_trace(marker = list(colors = c('grey', 'lightgreen')))
    
    mb
  })
  
  
  track_attribute3 <- spotify %>%
    select(track_name, loudness, tempo, duration) %>%
    distinct(track_name, .keep_all = TRUE) %>%
    pivot_longer(cols= loudness:duration,
                 names_to = "attribute",
                 values_to = "value")
  
  output$loud_hist <- renderPlotly({
    
    lh <- ggplot(data = track_attribute3 %>% filter(attribute=="loudness"), 
           mapping = aes(x = value)) + 
      geom_histogram(bins = input$bins_loud) +
      labs(x = "Value (dB)", y = "Frequency", title = "Frequency of Loudness")
    
    ggplotly(lh, width = 600)  # Adjust the height value as needed
  })
  
  output$loud_desc <- renderUI({
    text <- "
            <ul><li> Most songs have average volumes. </li></ul>"
    HTML(text)
  })
  
  output$tempo_hist <- renderPlotly({
    
    th <- ggplot(data = track_attribute3 %>% filter(attribute=="tempo"), 
           mapping = aes(x = value)) + 
      geom_histogram(bins = input$bins_tempo) +
      labs(x = "Value (BPM)", y = "Frequency", title = "Frequency of Tempo")
    
    ggplotly(th, width = 600)  # Adjust the height value as needed
  })
  
  output$tempo_desc <- renderText({
    text <- "<ul> <li> Most songs have around 100-150 beats per minute (BPM). </li></ul>"
    HTML(text)
  })
  
  output$dur_hist <- renderPlotly({
    
    dh <- ggplot(data = track_attribute3 %>% filter(attribute=="duration") %>% mutate(sec = value/1000), 
           mapping = aes(x = sec)) + 
      geom_histogram(bins = input$bins_dur) +
      labs(x = "Value (seconds)", y = "Frequency", title = "Frequency of Duration")
    
    ggplotly(dh, width = 600)  # Adjust the height value as needed
  })
  
  output$dur_desc <- renderText({
    text <- "<ul> <li> Most songs are around 3 minutes long, though there are some anomalies.  </li> </ul>"
    HTML(text)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)