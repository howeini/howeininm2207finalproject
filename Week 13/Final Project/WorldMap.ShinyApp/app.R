if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(wbstats)) install.packages("wbstats", repos = "http://cran.us.r-project.org")

library(magrittr)
library(rvest)
library(countrycode)
library(readxl)
library(wbstats)
library(plyr)
library(dplyr)
library(maps)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(shiny)
library(tidyverse)

# Read CSV file
raw <- read_csv("Jan2022_Spotify_Weekly_Top_200_Songs_Streaming_Data_by_country_view_view.csv")

# Remove unnecessary columns
spotify <- subset(raw, select = -c(X_1, artists_num, artist_genre, collab, album_num_tracks, pivot))

# Obtain ISO3 Codes
# Create a data frame
iso_codes <- data.frame(
  country = c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Bulgaria", "Canada", "Chile",
              "Colombia", "Costa Rica", "Cyprus", "Czech Republic", "Denmark", "Dominican Republic", "Ecuador", "Egypt",
              "El Salvador", "Estonia", "Finland", "France", "Germany", "Greece", "Guatemala", "Honduras",
              "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Korea",
              "Latvia", "Lithuania", "Luxembourg", "Malaysia", "Mexico", "Morocco", "Netherlands", "New Zealand",
              "Nicaragua", "Norway", "Panama", "Paraguay", "Peru", "Poland", "Portugal", "Romania", "Saudi Arabia",
              "Singapore", "Slovakia", "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", "Thailand", "Turkey",
              "United Arab Emirates", "United Kingdom", "Ukraine", "Uruguay", "United States", "Vietnam", "Philippines", 
              "South Korea", "UK", "USA"), # Names that are in World_data but not spotify
  ISO3 = c("ARG", "AUS", "AUT", "BEL", "BOL", "BRA", "BGR", "CAN", "CHL", "COL", "CRI", "CYP", "CZE", "DNK", "DOM",
           "ECU", "EGY", "SLV", "EST", "FIN", "FRA", "DEU", "GRC", "GTM", "HND", "HKG", "HUN", "ISL", "IND",
           "IDN", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MYS", "MEX", "MAR", "NLD", "NZL", "NIC", "NOR",
           "PAN", "PRY", "PER", "POL", "PRT", "ROU", "SAU", "SGP", "SVK", "ZAF", "ESP", "SWE", "CHE", "TWN", "THA", "TUR",
           "ARE", "GBR", "UKR", "URY", "USA", "VNM", "PHL",
           "KOR", "GBR", "USA")
)

# Add ISO3 Column to spotify df
spotify['ISO3'] <- iso_codes$ISO3[match(spotify$country, iso_codes$country)]

# top_tracks shows the track ranked #1 in every country for each of the 4 weeks in Jan
top_tracks <- spotify %>%
  filter(rank == 1) %>%
  select(track_name, artist_names, album_cover, ISO3, country, region, language, rank,  week, streams,
         danceability, energy, key, mode, loudness, speechiness, acousticness,
         instrumentalness, liveness, valence, tempo, duration) %>%
  arrange(week, country) %>%
  pivot_longer(cols = streams:duration,
               names_to = "attribute",
               values_to = "value") %>%
  distinct()



  
# Load World Data
library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

# Add ISO3 Column to world_data df
world_data["ISO3_reg"] <- iso_codes$ISO3[match(world_data$region, iso_codes$country)]
world_data["ISO3_subreg"] <- iso_codes$ISO3[match(world_data$subregion, iso_codes$country)]
world_data$ISO3 <- ifelse(!is.na(world_data$ISO3_subreg), world_data$ISO3_subreg, world_data$ISO3_reg)


# Next, it's time to define the function that we'll use for building our world maps. The inputs to this 
# function are the merged data frame, the world data containing geographical coordinates, and the data type, 
# period and indicator the user will select in the R Shiny app. We first define our own theme, *my_theme()* 
# for setting the aesthetics of the plot. Next, we select only the data that the user has selected to view, 
# resulting in *plotdf*. We keep only the rows for which the ISO3 code has been specified. For some countries 
# (e.g. Channel Islands in the childlessness data), this was not the case, as these are not contained in the 
# ISO code data. We then add the data the user wants to see to the geographical world data. Finally, we plot 
# the world map. The most important part of this plot is that contained in the *geom_polygon_interactive()* 
# function from the *ggiraph* package. This function draws the world map in white with grey lines, fills it 
# up according to the value of the data selected (either childlessness or gender gap rank) in a red-to-blue 
# color scheme set using the *brewer.pal()* function from the *RColorBrewer* package, and interactively shows 
# at the tooltip the ISO3 code and value when hovering over the plot.

worldMaps <- function(df, world_data, attribute, week){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "right",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$attribute == attribute & df$week == week,] 
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['attribute'] <- rep(attribute, nrow(world_data))
  world_data['week'] <- rep(week, nrow(world_data))
  world_data['value'] <- plotdf$value[match(world_data$ISO3, plotdf$ISO3)]
  world_data['track_name'] <- plotdf$track_name[match(world_data$ISO3, plotdf$ISO3)]
  world_data['language'] <- plotdf$language[match(world_data$ISO3, plotdf$ISO3)]
  world_data['artist_names'] <- plotdf$artist_names[match(world_data$ISO3, plotdf$ISO3)]
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'darkgrey', size = 0.1,
                             aes(x = long, y = lat, fill = value, group = group, 
                                 tooltip = paste("Country (Language):", region, "(",language,")",
                                              "</br>Top Song:", track_name,
                                              "</br>Artists:", artist_names,
                                              "</br>Value of", attribute, ":", value))) + 
    scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), na.value = 'white') + 
    labs(fill = attribute, color = attribute, title = NULL, x = NULL, y = NULL) + 
    my_theme()
  
  return(g)
}


global_top_tracks <- top_tracks %>% filter(country == "Global")


# Define UI 
ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML(
        "
        #global_text {
          padding: 6px 12px;
          white-space: pre-wrap;
        }
        "
      )
    )
  ),

    # Application title
    titlePanel("World Map"),

    bootstrapPage(
            selectInput("attribute",
                        label = "Select the attribute to be displayed",
                        choices = unique(top_tracks$attribute),
                        selected = "streams"),
            sliderInput("week",
                        label = "Drag to select the week to be displayed. Alternatively, click the Play button to loop the animation!",
                        min = min(unique(top_tracks$week)),
                        max = max(unique(top_tracks$week)),
                        value = min(unique(top_tracks$week)),
                        step = 7,
                        ticks = TRUE,
                        animate = animationOptions(interval = 8000, loop = TRUE),
                        width = '100%'),
            hr(),
            verbatimTextOutput("global_text"),
            br(),

          # Hide errors
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          
          # Output: interactive world map
          h6("Tip: Select the magnifying glass (middle option), and double click to zoom in! 
             When you're done examining, select the last option to reset your view."),
          girafeOutput("distPlot", width = "100%")
        )
    )




# Define server logic required to draw a histogram
server <- function(input, output) {

  # Create the interactive world map
  output$distPlot <- renderGirafe({
    x <- ggiraph(code = print(worldMaps(top_tracks, world_data, input$attribute, input$week)))
    x <- girafe_options(x, opts_zoom(min = 1, max = 20, duration = 1000),
                        opts_tooltip(css = "padding:10px;background-color:#333333;color:white;", opacity = 1))
  })
  
  
  output$global_text <- renderText({
    sel_attribute <- input$attribute
    sel_week <- input$week
    song_name <-  global_top_tracks %>%
      filter(attribute %in% as.vector(input$attribute)) %>%
      filter(week %in% as.vector(input$week)) %>%
      pull(track_name)
    
    attribute_value <- global_top_tracks %>%
      filter(attribute %in% as.vector(input$attribute)) %>%
      filter(week %in% as.vector(input$week)) %>%
      pull(value)
    
    artists <- global_top_tracks %>%
      filter(attribute %in% as.vector(input$attribute)) %>%
      filter(week %in% as.vector(input$week)) %>%
      pull(artist_names)
    
    output_text <- paste0("Based on the global charts, the top song in ", sel_week, " is ", song_name, " by ", artists,
                          ". For this song, the value of your selected attribute, ", sel_attribute, ", is ", attribute_value, ".")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

