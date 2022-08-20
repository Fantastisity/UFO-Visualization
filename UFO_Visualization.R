library(ggplot2)
library(dplyr)
library(wordcloud2)
library(sp)
library(rgdal)
library(ggrepel)
library(stringr)
library(shiny)
library(shinydashboard)
library(ggiraph)
library(tidyverse)
library(tidytext)
library(memoise)
library(RColorBrewer)
library(scales)
library(igraph)
library(tidyr)
library(ggraph)
library(ggthemes)
library(circlize)
library(gganimate)
library(gifski)
library(randomForest)
library(shinyWidgets)
library(fresh)
library(DT)

# Parse CSV file
df <- read.csv('./UFO_DATA.csv', stringsAsFactors = F)
# Parse OGR data(world map) to spatial vector object
world_data <- readOGR(dsn = '50m_cultural', layer = 'ne_50m_admin_0_countries')
# Re-project spatial vector object to latitude and longitude coordinate reference system
world_data <- spTransform(world_data, CRS("+proj=longlat"))
# Converts spatial vector object to a dataframe, use ISO_A2 for splitting the regions
world_data <- fortify(world_data, region = "ISO_A2" )
# Map unique country code to a new dataframe
countries <- data.frame(unique(world_data$id)[-1])
colnames(countries) <- c("country_code")
# Create a spatial vector object from dataframe with longitude and latitude as coordinates
coordinates(df) <- ~longitude + latitude
# Specify the coordinate system for the spatial vector object 
proj4string(df) <- CRS("+proj=longlat")
# Re-project spatial vector object to latitude and longitude coordinate reference system
df <- spTransform(df, CRS(proj4string(df)))
# Revert back the spatial vector object to dataframe
df <- data.frame(df)
# Encode strings to integers for inputting into classification model 
df$country_map <- as.integer(as.factor(df$country))
df$season_map <- as.integer(as.factor(df$season))
df$shape_map <- as.factor(df$shaping)
# Create pie chart
pie <- 
  # Count occurrence rate of each country
  df %>% select(country) %>% group_by(country) %>% summarise(cnt = n()) %>%
  # Calculate proportion in pie chart(individual occurrence rate / total occurrence rate)
  mutate(percentage = percent(cnt / sum(cnt)))%>%
  # Create tip box to display country and its relative proportion
  mutate(tip = sprintf("%s: %s", country, percentage)) %>% 
  # Construct pie chart with tip box, fill color based on country
  ggplot(aes(x = "", y = cnt, fill = country, tooltip = tip)) + 
  # Create interactive bar chart with mouse hovering event 
  geom_bar_interactive(width = 1, stat = "identity") +
  # Convert bar chart to pie chart
  coord_polar('y', start = 0) + 
  theme_void() + 
  # Use color palette for the fill
  scale_fill_brewer(palette = "PuBuGn") + 
  # Display country name on the relevant parts within pie chart
  geom_label_repel(aes(label = country), size=4, 
                   position = position_stack(vjust = 0.5)) + 
  # Remove legend
  guides(fill = 'none')

# Create animated density map if it hasn't been created yet
if (!file.exists("./world_map.gif")) {
  world_map <- 
    ggplot() + 
    theme_map() +
    # Apply theme to the world map
    theme(
      panel.border = element_blank(),
      plot.background = element_rect(fill = 'seashell3'),
      legend.position = "none") +
    # Use the world_data we've created earlier to create the world map 
    geom_map(data = countries, aes(map_id = country_code), 
             size = 0.2, map = world_data) +
    # Make world map bigger by expanding the plot limits
    expand_limits(x = world_data$long, y = world_data$lat) + 
    # Add dots based on latitude and longitude values
    geom_point(data = df, aes(longitude, latitude, col = country), 
               size=3, alpha=0.8) +
    # Animation of the dots based on year 
    transition_time(year)
  # Save the animated map to current directory as a GIF with 15s duration
  anim_save("world_map.gif", animate(world_map, duration = 15, width = 620, height = 500, fps = 12,
                                     renderer = gifski_renderer()))
}

bigram <- 
  # Extract bigrams within comments
  df %>% unnest_ngrams(word, comments, n = 2) %>% 
  # Sort unique bigrams based on its occurrence rate, then filter out bigrams
  # with occurrence rate <= 500 and length <= 2, and separate each bigram to words
  count(word, sort=T) %>% separate(word, c("w1", "w2"), sep = " ") %>%
  filter(nchar(w1) > 2)%>%
  filter(nchar(w2) > 2)%>%
  filter(n > 500) 

network <-
  # Create a network diagram from the bigram dataframe
  bigram %>% graph_from_data_frame() %>%
  # Use Fruchterman-Reingold layout for the diagram
  ggraph(layout = "fr") +
  # Plot graph edges with arrows at the end of each edge
  geom_edge_link(aes(edge_alpha = n), show.legend = F, 
                 arrow = grid::arrow(type = "open", 
                                     length = unit(.05, "inches"))) +
  # Associate each word with a point as graph vertex
  geom_node_point(color = "tomato", size=1) +
  # Display word next to its vertex
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void()

# Plot chord diagram using the bigram dataframe
plot_chord <- function(inp) {
  # Set the gap between each word
  circos.par(gap.after = 4, points.overflow.warning = F)
  # Add grid for the chord diagram with a single track
  chordDiagram(inp, annotationTrack = "grid", preAllocateTracks = 1)
  # Alter the coordinates of the words
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    # Get current coordinates of the words
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    # Get words
    sector.name = get.cell.meta.data("sector.index")
    # Alter the coordinates of the words
    circos.text(mean(xlim), ylim[1] + .2, sector.name, facing = "clockwise", 
                niceFacing = T, adj = c(0, degree(2)), cex = 0.8)
  }, bg.border = NA)
  # Reset layout settings back to default
  circos.clear()
}

# Function for plotting top states/cities
# f: 0 for state, 1 for city
# yr_bgn, yr_fin: corresponds to the maximum/minimum year on the shiny slider input
# num: corresponds to the number of states/cities on the shiny slider input
# country: corresponds to the countries in the shiny drop down list
plot_spatialInfo <- function(df, f = 0, yr_bgn, yr_fin, country = NULL, num) {
  # Default to display the top states/cities of the world
  if (is.null(country)) df.tmp <- df
  # If country is specified, then extract a new dataframe for that country only
  else df.tmp <- df[df$country == country, ]
  if (!f) {
    tmp <-  
      df.tmp %>%
      select(state) %>% group_by(state) %>% summarise(cnt = n()) %>%
      # Select top num states
      arrange(desc(cnt)) %>% slice(1 : num) %>%
      # Tip box
      mutate(tip = cnt)
    # Plot the states in decreasing order with X and Y coordinates fliped
    tmp <- tmp %>% ggplot(aes(x = reorder(state, cnt), y = cnt, fill = cnt, tooltip = tip)) +
      coord_flip()
  } else { # Same process for cities
    tmp <-  
      df.tmp %>%
      select(city) %>% group_by(city) %>% summarise(cnt = n()) %>%
      arrange(desc(cnt)) %>% slice(1 : num) %>%
      mutate(tip = cnt)
    tmp <- tmp %>% ggplot(aes(x = reorder(city, cnt), y = cnt, fill = cnt, tooltip = tip)) +
      coord_flip()
  }
  tmp <- 
    tmp +
    # Bar chart with color saturation
    geom_bar_interactive(stat = 'identity') +
    scale_fill_gradient(low="#D0E9ED",high="#87A0A3") + 
    theme_minimal() + 
    theme(
      # Centering plot title
      plot.title = element_text(hjust = 0.5),
      # Change state/city name size
      axis.text.x = element_text(size = 10),
      # Remove plot grids for a transparent display
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'azure')
    ) +
    xlab(ifelse(!f, "State", "City")) + 
    ylab("No. of Sightings") + 
    guides(fill = "none") + 
    ggtitle(sprintf("Top %d %s in %s\n between year %d and %d", 
                    num, ifelse(!f, "States", "Cities"), 
                    ifelse(is.null(country), "the World", country),
                    yr_bgn, yr_fin))
  return (tmp)
}

# Function for plotting temporal trends
# f: 0 for yearly trends, 1 for duration, 2 for seasonal trends, 3 for hourly trends
plot_timeInfo <- function(df, f = 0, yr_bgn, yr_fin, country = NULL, C = 1) {
  if (is.null(country)) df.tmp <- df
  else df.tmp <- df[df$country == country, ]
  if (!f) {
    if (C) (
      tmp <- 
        df.tmp %>% select(year) %>% group_by(year) %>% summarise(cnt = n()) %>%     
        ggplot(aes(x = year, y = cnt)) + 
        geom_line(col = brewer.pal(9, "PuBuGn")[9]) + 
        xlab("Year") + ylab("No. of Sightings") + 
        # Re-scale the x-axis with break size equaling 6 years
        scale_x_continuous(breaks = seq(yr_bgn, yr_fin, 6)) + 
        ggtitle(sprintf("Yearly Sightings in %s between year %d and %d", 
                        ifelse(is.null(country), "the World", country),
                        yr_bgn, yr_fin)))
    else {
      tmp <-
        df %>% select(year, country) %>% group_by(year, country) %>% summarise(cnt = n()) %>%     
        ggplot(aes(x = year, y = cnt, color = country)) + 
        geom_line() + scale_color_brewer(palette="Paired") +
        xlab("Year") + ylab("No. of Sightings") + 
        # Re-scale the x-axis with break size equaling 6 years
        scale_x_continuous(breaks = seq(yr_bgn, yr_fin, 8)) + 
        ggtitle(sprintf("Yearly Sightings stacked between year %d and %d",
                        yr_bgn, yr_fin))
    }
  } else if (f == 1) {
    if (C) (
      tmp <- 
        df.tmp %>% 
        ggplot(aes(duration..seconds.)) + 
        geom_density(fill = 'mediumaquamarine') + 
        xlab("Seconds") + ylab("Density") +
        scale_x_continuous(breaks = seq(0, 1440, 180)) + 
        ggtitle(sprintf("Duration Density in %s between year %d and %d", 
                        ifelse(is.null(country), "the World", country),
                        yr_bgn, yr_fin)))
    else {
      tmp <- 
        df %>%
        ggplot(aes(duration..seconds., fill = fct_rev(country))) + 
        geom_density(alpha = 0.3) + guides(fill=guide_legend("Country")) +
        xlab("Seconds") + ylab("Density") + scale_fill_brewer(palette="PRGn") +
        scale_x_continuous(breaks = seq(0, 1440, 180)) + 
        ggtitle(sprintf("Duration Density stacked between year %d and %d",
                        yr_bgn, yr_fin))
    }
  } else if (f == 2) {
    tmp <- 
      df.tmp %>% ggplot(aes(x = year, y = season)) + 
      geom_point(col = brewer.pal(11, "PRGn")[10]) + 
      xlab("Year") + ylab("Season") +  
      ggtitle(sprintf("Seasonal Changes in %s between year %d and %d", 
                      ifelse(is.null(country), "the World", country),
                      yr_bgn, yr_fin))
  } else {
    tmp <- 
      df.tmp %>% 
      ggplot(aes(x = hour, y = year)) + 
      coord_flip() +
      geom_point(col = brewer.pal(11, "PRGn")[10]) + 
      xlab("Hour") + ylab("Year") +
      scale_x_continuous(breaks = seq(1, 24, by = 1)) +
      ggtitle(sprintf("Hourly Changes in %s between year %d and %d", 
                      ifelse(is.null(country), "the World", country),
                      yr_bgn, yr_fin))
  }
  tmp <- 
    tmp + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'azure')
    )
  return (tmp)
}

# Calculation utility function
# f: 0 for year, 1 for duration, 2 for country, 3 for season, 4 for hour
# C: flag used for multiple purposes
# If the result is Null, which indicates there is no valid data for calculation,
# then 'Unavailable' is returned
calculation <- function(df, f, C, country = NULL) {
  if (is.null(country)) df.tmp <- df
  else df.tmp <- df[df$country == country, ]
  if (!f) {
    if (!C) { # Calculate average number of sightings
      res <- df.tmp %>% select(year) %>% group_by(year) %>% tally() %>% ungroup() %>% 
        summarise(avg = mean(n)) %>% pull(avg) %>% as.integer()
      if (is.na(res)) res <- "Unavailable" # No sightings 
    } else if (C == 1) { # Find year with maximum number of sightings, return the year and sightings
      res <- df.tmp %>% select(year) %>% group_by(year) %>% tally() %>% 
        ungroup() %>% slice_max(n)
      if (!dim(res)[1]) res <- c("", "Unavailable")
      else if (dim(res)[1] > 1) {
        # If there are multiple years with the same max value, return 'multiple years'
        res <- c("multiple years", res[1, "n"])
      } else res <- c(paste("year", res["year"]), res["n"])
    }
  } else if (f == 1) {
    if (!C) { # Calculate average duration
      res <- as.integer(sum(df.tmp[, "duration..seconds."]) / dim(df.tmp)[1])
    } else if (C == 1) { # Calculate maximum duration
      res <- max(df.tmp[, "duration..seconds."])
      if (is.na(res)) res <- "Unavailable"
      else res <- paste(res, "s", sep = '')
    } else { # Calculate minimum duration
      res <- min(df.tmp[, "duration..seconds."])
      if (is.na(res)) res <- "Unavailable"
      else res <- paste(res, "s", sep = '')
    }
  } else if (f == 2){ # Find maximum and minimum number of sightings group by country
    res <- df.tmp %>% select(country) %>% group_by(country) %>% tally() %>% ungroup()
    if (!C) {
      res <- res %>% slice_max(n, n = 1)
      res <- c(res["country"], res["n"])
    } else {
      res <- res %>% slice_min(n, n = 1)
      res <- c(res["country"], res["n"])
    }
  } else if (f == 3) { # Find maximum and minimum number of sightings group by season
    res <- df.tmp %>% select(season) %>% group_by(season) %>% tally() %>% ungroup()
    if (!C) res <- res %>% slice_max(n, n = 1)
    else res <- res %>% slice_min(n, n = 1)
    if (!dim(res)[1]) res <- "Unavailable"
    else res <- res[1, "season"]
  } else if (f == 4){ # Find maximum and minimum number of sightings group by hour
    res <- df.tmp %>% select(hour) %>% group_by(hour) %>% tally() %>% ungroup()
    if (!C) res <- res %>% slice_max(n, n = 1)
    else res <- res %>% slice_min(n, n = 1)
    if (!dim(res)[1]) res <- "Unavailable"
    else res <- paste(res[1, "hour"], ":00", sep = '')
  } else if (f == 5){ # Find state & city with maximum number of sightings 
    state_res <- df.tmp %>% select(state) %>% group_by(state) %>% 
      tally() %>% ungroup() %>% slice_max(n, n = 1)
    city_res <- df.tmp %>% select(city) %>% group_by(city) %>% 
      tally() %>% ungroup() %>% slice_max(n, n = 1)
    if (!dim(state_res)[1]) state_res <- "Unavailable"
    else state_res <- state_res[1, "state"]
    if (!dim(city_res)[1]) city_res <- "Unavailable"
    else city_res <- city_res[1, "city"]
    res <- c(state_res, city_res)
  } else { # Find most common shape and bigram
    shape_res <- df.tmp %>% select(shaping) %>% group_by(shaping) %>% 
      tally() %>% ungroup() %>% slice_max(n, n = 1)
    bigram_res <- df.tmp %>% unnest_ngrams(word, comments, n = 2) %>% 
      count(word, sort=T) %>% slice_max(n, n = 1)
    if (!dim(shape_res)[1]) shape_res <- "Unavailable"
    else shape_res <- shape_res[1, "shaping"]
    if (!dim(bigram_res)[1]) bigram_res <- "Unavailable"
    else bigram_res <- bigram_res[1, "word"]
    res <- c(shape_res, bigram_res)
  }
  return (res)
}

# Plot semantic information
# f: 0 for shape frequency distribution, 1 for wordcloud
plot_descriptions <- function(df, f = 0, yr_bgn, yr_fin, country = NULL) {
  if (is.null(country)) df.tmp <- df
  else df.tmp <- df[df$country == country, ]
  if (!f) {
    tmp <- 
      df.tmp %>% select(shaping) %>% group_by(shaping) %>% 
      summarise(cnt = n()) %>% mutate(tip = cnt) %>%
      # Decreasing order of the shape
      ggplot(aes(x = reorder(shaping, -cnt), y = cnt, fill = cnt, 
                 tooltip = tip)) + 
      geom_bar_interactive(stat = 'identity') +
      # Color saturation
      scale_fill_gradient(low="#BCE7D6",high="#99C3B3") + 
      theme_minimal() + theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'azure')
      ) + 
      xlab("Shape") + 
      ylab("No. of Times") + 
      scale_x_discrete(guide = guide_axis(check.overlap = T)) + 
      guides(fill = "none") + 
      ggtitle(sprintf("Common Shapes by observers in %s\n between year %d and %d", 
                      ifelse(is.null(country), "the World", country),
                      yr_bgn, yr_fin))
  } else {
    words <-
      df.tmp %>% select(comments) %>% 
      # Extract unigrams within comments and reorder each unique unigram with occurrence rate
      unnest_ngrams(word, comments, n = 1) %>%
      count(word, sort = T)
    # Create wordcloud with unigrams changing in size based on occurrence rate 
    tmp <- wordcloud2(words,shape = 'star', backgroundColor = 'rgba(74,148,147,0.09)')
  } 
  return (tmp)
}

# Train the random forest model for predicting shapes given year, duration, season, country and day
# using 200 decision trees
rf <- randomForest(shape_map~year+duration..seconds.+season_map+country_map+day,
                   data = df, ntree=200, importance = T)

# Predefined theme for shiny dashboard
db_theme <- create_theme(
  adminlte_color( # Global color for the dashboard
    light_blue = "#507579"
  ),
  adminlte_sidebar( # Side bar (sub)menu color
    width = "243px", # Width of the side bar
    dark_bg = "#7E989B",
    dark_submenu_bg = "#9AB9BD", # Default background color
    dark_submenu_color = "#EDF5F9", # text color
    dark_submenu_hover_color = "#EDF5F9", # text hover color
    dark_hover_bg = "#4A5659",
    dark_color = "#EDF5F9",
    dark_hover_color = "#EDF5F9"
  ),
  adminlte_global( # Content background color
    content_bg = "#C8D9DB"
  )
)

# Color and font size for the shiny dropdown list items
# num: number of items in the dropdown list
set_dropdown_css <- function(num) {
  dropdown_css <- paste0("background:", rep(c("#7D9B9F"), num), ";")
  dropdown_css <- paste0(dropdown_css,"font-weight:600;")
  return (dropdown_css)
}

# Main dashboard UI
ui <- dashboardPage(
  # Header element
  dashboardHeader(
    title = 'UFO Sightings'
  ),
  # Side bar element
  dashboardSidebar(
    # Set skin of slider input
    chooseSliderSkin("Square"),
    # 5 Main menus: home page, 3 main categories, and a report page for displaying summary
    # Each of the 3 main categories contains several submenus
    sidebarMenu(id="menus",
                menuItem("Home", tabName = "main", icon = icon("home")),
                menuItem("Spatial Information", tabName = "spatial", icon = icon('globe'),
                         menuSubItem("Country", tabName = 'country', icon = icon('globe-americas')),
                         menuSubItem("State", tabName = "state", icon = icon('map-pin')),
                         menuSubItem("City", tabName = "city", icon = icon('city'))
                ),
                menuItem("Temporal Trends", tabName = "time", icon = icon('clock'),
                         menuSubItem("Yearly", tabName = "year", icon = icon('calendar-alt')),
                         menuSubItem("Duration", tabName = "duration", icon = icon('stopwatch-20')),
                         menuSubItem("Seasonality", tabName = "season", 
                                     icon = icon('umbrella-beach')),
                         menuSubItem("Hourly", tabName = "hour", icon = icon('stopwatch'))),
                menuItem("Descriptions", tabName = "desc", icon = icon('comments'),
                         menuSubItem("Shapes", tabName = "shape", icon = icon("shapes")),
                         menuSubItem("Shape Prediction", tabName = "shape_pred", icon = icon("star")),
                         menuSubItem("Word Cloud", tabName = "wordcloud", icon = icon("quote-left")),
                         menuSubItem("Word Connection", tabName = "phraseconnection", icon = 
                                       icon("project-diagram"))),
                menuItem("Report", tabName = 'repo', icon = icon('table')),
                # Conditional panels used for displaying certain plots/comments/user inputs
                # only when user clicks one of the specified menu items
                conditionalPanel( # Click the switch to convert to plot stacked by country
                  'input.menus === "year" || 
         input.menus === "duration"',
                  fluidPage(
                    materialSwitch(
                      'switch', label = "To Stacked", inline = T, status = "success"
                    )
                  )
                ),
                conditionalPanel(
                  '!input.switch && input.menus === "year" ||
             !input.switch && input.menus === "duration" ||
             input.menus === "state" || 
             input.menus === "city" || 
             input.menus === "season" ||
             input.menus === "hour" ||
             input.menus === "shape" ||
             input.menus === "wordcloud" ||
             input.menus === "repo"',
                  pickerInput( # Dropdown list for selecting country of interest, defaulted to world
                    'A', label = p(strong("Select Country"), style = 'font-size:17px'),
                    choices = list(
                      "World" = 1,
                      "United States" = 2,
                      "Australia" = 3,
                      "Germany" = 4,
                      "United Kingdom" = 5,
                      "Canada" = 6,
                      "Unclear" = 7
                    ), selected = 1,
                    choicesOpt = list(style = set_dropdown_css(7)) # Set dropdown list item style
                  )
                ),
                conditionalPanel(
                  'input.menus === "year" ||
             input.menus === "duration" ||
             input.menus === "state" || 
             input.menus === "city" || 
             input.menus === "season" ||
             input.menus === "hour" ||
             input.menus === "shape" ||
             input.menus === "wordcloud" ||
             input.menus === "repo"',
                  sliderInput( # Slider input for selecting year range
                    'B', label = p(strong("Select Year Range"), style = 'font-size:17px'),
                    min(df$year), max(df$year),
                    value = range(df$year),
                    step = 1
                  )
                ),
                conditionalPanel(
                  'input.menus === "state"',
                  sliderInput( # Slider input for selecting number of states
                    'C', label = p(strong("Select Number of States"), style = 'font-size:17px'),
                    1, 50,
                    value = 10,
                    step = 1
                  )
                ),
                conditionalPanel(
                  'input.menus === "city"',
                  sliderInput( # Slider input for selecting number of cities
                    'D', label = p(strong("Select Number of Cities"), style = 'font-size:17px'),
                    1, 50,
                    value = 10,
                    step = 1
                  )
                ),
                # Footnotes displayed in sidebar
                conditionalPanel(
                  'input.menus === "season" ||
           input.menus === "hour"',
                  fluidPage( 
                    p("Note: UTC timezone", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "repo"',
                  fluidPage( 
                    p("Note: columns correspond to the", br(), "most frequent/average values", 
                      style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "country"',
                  prettyRadioButtons( # Radio buttons for selecting plot type of, defaulted to pie chart
                    'E', label = p(strong("Select Plot Type"), style = 'font-size:17px'),
                    choices = list(
                      "Pie Chart" = 1,
                      "Map" = 2
                    ), selected = 1 , inline = T, fill = T, bigger = T, status = "success", 
                    animation = "smooth"
                  )
                ),
                # Comments displayed in sidebar(some are displayed for one particular country only)
                conditionalPanel(
                  'input.menus === "country" && input.E === "1"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("From pie chart, we can see that", br(), "Us happens to be the country", br(), 
                      "having most number of sightings,", br(), "with 87% of all the sightings", br(), 
                      "around the world", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "country" && input.E === "2"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The map indicates that a large", br(), "proportion of the UNCLEAR", br(), 
                      "locations(blue) are situated in", br(), "Europe and Asia", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "state" && input.A === "1"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("Observe that most of the top", br(), "states (excluding 'unclear') are all", br(), "located within US, also they all", br(), "happen to be around the coast", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "year" && !input.switch && input.A === "1"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The year with most sightings is", br(), "2012. Interestingly, it was also the", br(), "year believed to be end of the world,", br(), "therefore we cannot be certain", br(), "whether UFOs truly appeared more", br(), "frequent in that year, or people", br(), "were just being overly exaggurative", br(), "and made false claims.", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "year" && input.switch',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("From the plot, one thing in common", br(), "is that the number of sightings", br(), "in all countries seems to increase", br(), "rapidly starting from 1990-ish,", br(), "this is probably due to the internet", br(), "is made available to the world", br(), "at that time, so that it was easier", br(), "for people to pass information.", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "duration" && input.switch',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("Despite the density differences, all", br(), "the curves tend to follow a similar", br(), "distribution", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "duration" && !input.switch && input.A === "1"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The duration appears to be in", br(), "a multimodal shape, ie. multiple", br(), "peak values, 
               with the highest peak", br(), "located at around 100 seconds,", br(), 
                      "which corresponds to roughly 2", br(), "minutes", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "season" && input.A === "2"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The most frequent season in US", br(), "is summer. Take into consideration", br(), "that the top states are around the", br(), "coast, we may argue that people", br(), "might mistaken UFOs for mirages", br(), "due to water vapor", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "hour" && input.A === "2"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The most frequent hour in US", br(), "is 5pm(4h ahead of UTC time),", br(), 
                      "which is close to sunset", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "shape" && input.A === "1"',
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("The most common shapes are", br(), "circular and shiny, which is", br(), 
                      "how we picture UFOs to be", style = 'font-size:15px;')
                  )
                ),
                conditionalPanel(
                  'input.menus === "phraseconnection"',
                  prettyRadioButtons( # Radio buttons for selecting plot type, defaulted to network diagram
                    'F', label = p(strong("Select Plot Type"), style = 'font-size:17px'),
                    choices = list(
                      "Network" = 1,
                      "Chord" = 2
                    ), selected = 1 , inline = T, fill = T, bigger = T, status = "success", animation = "smooth"
                  ),
                  fluidPage(
                    p(strong("Comment"), style = 'font-size:17px'),
                    p("From the most frequent phrases,", br(), "something interesting to note is", 
                      br(), "that the UFOs tend to be appear", br(), "in various unusual colors(green,",
                      br(), "red, orange)",
                      style = 'font-size:15px;')
                  )
                )
    )
  ),
  
  dashboardBody(
    # Set color for information boxes
    tags$style(
      ".small-box.bg-blue { background-color: #578483 !important; color: #F4F4F9 !important; }"
    ),
    # Set color of both selected and hovered sub menu items
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu .treeview-menu li.active a {background-color: #627377 !important;}")),
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {background-color: #627377 !important;}")),
    # Change the selected color for dropdown list items
    tags$style(HTML(".selected {background-color:#475253 !important;}")),
    # Use previously defined dashboard theme
    use_theme(db_theme),
    tabItems(
      # ---------------------        home page        ---------------------
      tabItem(
        tabName = 'main',
        fluidPage(
          mainPanel(
            width = "3px",
            h2(strong("Brief Intro")),
            # Introductions
            p("the visualizations are arranged into 3 main categories:", style = "font-size:17px;"),
            br(),
            tags$ol(
              tags$li(strong("Spatial Information: ", style = 'color:#6038DE'), 
                      span(em("shows the geographical info of the sightings, ie. which country/state/city 
                      has the most number of sightings")), style = "font-size:17px;"),
              tags$li(strong("Temporal Trends: ", style = 'color:#6038DE'), span(em("provides visuals 
              on yearly/seasonal/hourly trends, as well as density distribution of the time duration 
               of the sightings")), style = "font-size:17px;"),
              tags$li(strong("Descriptions: ", style = 'color:#6038DE'), span(em("examines the common 
              shaping of the UFOs and the descriptions of the sightings(ie. common phrases)")), 
                      style = "font-size:17px;")
            ),
            br(),
            p(strong("Reminder: for pie chart and bar charts, users may hover their mouse on plots to
              display certain statistical information"))
          )
        )
      ),
      # ---------------------        spatial information        ---------------------
      tabItem(
        tabName = 'country',
        fluidPage(
          splitLayout(
            # Information boxes for displaying maximum/minimum number of sightings grouped by country
            valueBoxOutput("topCountry", width = NULL),
            valueBoxOutput("botCountry", width = NULL)
          ),
          br(),
          # Plot pie chart/animated density map given by selection in the dropdown list
          conditionalPanel(
            'input.E == 1',
            # Add spinner for plot loading animation
            # ggiraphOutput: for outputting interative ggplots
            addSpinner(ggiraphOutput("pie"), spin = 'folding-cube', color = '#5A706F')
          ),
          conditionalPanel(
            'input.E == 2',
            # display the GIF we've previously created
            imageOutput("map")
          )
        )
      ),
      # Plot states and cities
      tabItem(
        tabName = 'state',
        fluidPage(
          addSpinner(ggiraphOutput("state"), spin = 'folding-cube', color = '#5A706F')
        )
      ),
      tabItem(
        tabName = 'city',
        fluidPage(
          addSpinner(ggiraphOutput("city"), spin = 'folding-cube', color = '#5A706F')
        )
      ),
      # ---------------------        temporal information        ---------------------
      tabItem(
        tabName = 'year',
        fluidPage(
          conditionalPanel(
            "input.switch == 0",
            splitLayout(
              # Information boxes for displaying average/maximum number of sightings
              valueBoxOutput("avgYear", width = NULL),
              valueBoxOutput("maxYear", width = NULL)
            ),
            hr()),
          addSpinner(plotOutput("year"), spin = 'folding-cube', color = '#5A706F'),
        )
      ),
      tabItem(
        tabName = 'duration',
        fluidPage(
          conditionalPanel(
            "input.switch == 0",
            splitLayout(
              # Information boxes for displaying average/maximum/minimum duration of sightings
              valueBoxOutput("avgDura", width = NULL),
              valueBoxOutput("maxDura", width = NULL),
              valueBoxOutput("minDura", width = NULL)
            ),
            hr()),
          addSpinner(plotOutput("duration"), spin = 'folding-cube', color = '#5A706F')
        )
      ),
      tabItem(
        tabName = 'season',
        fluidPage(
          splitLayout(
            # Information boxes for displaying maximum/minimum numer of sightings grouped by season
            valueBoxOutput("maxSeason", width = NULL),
            valueBoxOutput("minSeason", width = NULL)
          ),
          hr(),
          addSpinner(plotOutput("season"), spin = 'folding-cube', color = '#5A706F')
        )
      ),
      tabItem(
        tabName = 'hour',
        fluidPage(
          splitLayout(
            # Information boxes for displaying maximum/minimum numer of sightings grouped by hour
            valueBoxOutput("maxHour", width = NULL),
            valueBoxOutput("minHour", width = NULL)
          ),
          hr(),
          addSpinner(plotOutput("hour"), spin = 'folding-cube', color = '#5A706F')
        )
      ),
      # ---------------------        semantic information        ---------------------
      tabItem(
        tabName = 'shape',
        fluidPage(
          # Shape frequency distribution
          ggiraphOutput("shape")
        )
      ),
      tabItem(
        tabName = 'wordcloud',
        # Wordcloud
        wordcloud2Output("wc")
      ),
      # Plot network diagram/chord diagram given by selection in the dropdown list
      tabItem(
        tabName = 'phraseconnection',
        fluidPage(
          conditionalPanel(
            'input.F == 1',
            plotOutput("net")
          ),
          conditionalPanel(
            'input.F == 2',
            plotOutput("chord")
          )
        )
      ),
      tabItem( # Predict shape using random forest
        tabName = "shape_pred",
        fluidPage(
          # User inputs for classification model with hints
          textInput("predyear", "Enter year: ", 
                    placeholder = "Hint: enter a year between 1906 and 2014"),
          textInput("preddura", "Enter duration: ",
                    placeholder = "Hint: enter a value between 0 and 1440"),
          textInput("predseason", "Enter season: ",
                    placeholder = "Hint: one of {Spring, Summer, Autumn, Winter}"),
          textInput("predcountry", "Enter country: ",
                    placeholder = "Hint: one of {US, AU, CA, GB, DE, UNCLEAR}"),
          textInput("predday", "Enter day: ",
                    placeholder = "Hint: enter a value between 1 and 31"),
          # Click the button to make prediction
          actionBttn("Pred", label = "Get Shape", icon = icon("canadian-maple-leaf"), style = 
                       "material-flat", size = "sm", color = "success"),
          div(style="padding-left:14px;width:285px;", 
              fluidRow(verbatimTextOutput("predShape")))
        )
      ),
      # ---------------------        report        ---------------------
      tabItem(
        tabName = 'repo',
        fluidPage(
          p(strong("Drag year range then click generate report to alter table(scroll left to view more columns)")),
          br(),
          # Click generate report to display formatted table
          actionBttn("Gen", label = "Generate Report", icon = icon("file-csv"), style = 
                       "material-flat", size = "sm", color = "success"),
          br(),br(),
          DTOutput("gen_report"),
        )
      )
    )
  )
)

# Shiny server
server <- function(input, output) {
  # Create set of variables for reacting to dynamic user inputs
  State <- reactiveValues(data = NULL)
  City <- reactiveValues(data = NULL)
  
  Year <- reactiveValues(data = NULL)
  Avg_year <- reactiveValues(data = NULL)
  Max_year <- reactiveValues(data = NULL)
  
  Duration <- reactiveValues(data = NULL)
  Avg_dura <- reactiveValues(data = NULL)
  Max_dura <- reactiveValues(data = NULL)
  Min_dura <- reactiveValues(data = NULL)
  
  Season <- reactiveValues(data = NULL)
  Max_season <- reactiveValues(data = NULL)
  Min_season <- reactiveValues(data = NULL)
  
  Hour <- reactiveValues(data = NULL)
  Max_hour <- reactiveValues(data = NULL)
  Min_hour <- reactiveValues(data = NULL)
  
  Shape <- reactiveValues(data = NULL)
  WordCloud <- reactiveValues(data = NULL)
  Predshape <- reactiveValues(data = NULL)
  
  Report <- reactiveValues(data = NULL)
  
  # Utility functions for generating dynamic 1) spatial/temporal/semantic information plots 2) calculations
  # ind: index of the selected country in the dropdown country list
  # num: number of states/cities given by slider input
  spatial_util <- function(df, f, ind, num) {
    if (ind == 1) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], NULL, num))))
    if (ind == 2) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'US', num))))
    if (ind == 3) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'AU', num))))
    if (ind == 4) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'DE', num))))
    if (ind == 5) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'GB', num))))
    if (ind == 6) 
      return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'CA', num))))
    return (ggiraph(code = print(plot_spatialInfo(df, f, input$B[1], input$B[2], 'UNCLEAR', num))))
  }
  
  time_util <- function(df, f, ind, C = 1) {
    if (ind == 1) return (plot_timeInfo(df, f, input$B[1], input$B[2], NULL, C))
    if (ind == 2) return (plot_timeInfo(df, f, input$B[1], input$B[2], 'US'))
    if (ind == 3) return (plot_timeInfo(df, f, input$B[1], input$B[2], 'AU'))
    if (ind == 4) return (plot_timeInfo(df, f, input$B[1], input$B[2], 'DE'))
    if (ind == 5) return (plot_timeInfo(df, f, input$B[1], input$B[2], 'GB'))
    if (ind == 6) return (plot_timeInfo(df, f, input$B[1], input$B[2], 'CA'))
    return (plot_timeInfo(df, f, input$B[1], input$B[2], 'UNCLEAR'))
  }
  
  desc_util <- function(df, f, ind) {
    if (ind == 1) return (plot_descriptions(df, f, input$B[1], input$B[2]))
    if (ind == 2) return (plot_descriptions(df, f, input$B[1], input$B[2], 'US'))
    if (ind == 3) return (plot_descriptions(df, f, input$B[1], input$B[2], 'AU'))
    if (ind == 4) return (plot_descriptions(df, f, input$B[1], input$B[2], 'DE'))
    if (ind == 5) return (plot_descriptions(df, f, input$B[1], input$B[2], 'GB'))
    if (ind == 6) return (plot_descriptions(df, f, input$B[1], input$B[2], 'CA'))
    return (plot_descriptions(df, f, input$B[1], input$B[2], 'UNCLEAR'))
  }
  
  calc_util <- function(df, f, C, ind) {
    if (ind == 1) return (calculation(df, f, C))
    if (ind == 2) return (calculation(df, f, C, 'US'))
    if (ind == 3) return (calculation(df, f, C, 'AU'))
    if (ind == 4) return (calculation(df, f, C, 'DE'))
    if (ind == 5) return (calculation(df, f, C, 'GB'))
    if (ind == 6) return (calculation(df, f, C, 'CA'))
    return (calculation(df, f, C, 'UNCLEAR'))
  }
  
  # Utility function for predicting shapes using random forest model
  shape_classifier <- function(year, duration, season, country, day) {
    # Normalize user inputted season and country
    season <- str_to_title(tolower(season))
    country <- toupper(country)
    # Generate test data
    xtest <- list(year, duration, season, country, day)
    names(xtest) <- c("year", "duration..seconds.", "season_map", "country_map", "day")
    # Find the corresponding encoded values of the string inputs
    xtest$country_map <- df[which(df$country == xtest$country), "country_map"][1]
    xtest$season_map <- df[which(df$season == xtest$season), "season_map"][1]
    # Make prediction
    pred <- predict(rf, xtest)
    return (toString(pred))
  }
  
  # Utility function for generating tabular report
  report_generator <- function(df) {
    # Create an empty dataframe where each row corresponds to a particular country
    report <- data.frame(matrix(NA, nrow = 0, ncol = 10))
    # Columns: 
    #   1. country name
    #   2. total number of sightings
    #   3. the state with most sightings of the country
    #   4. the city with most sightings of the country
    #   5. the year where most sightings occurred
    #   6. the season where most sightings occurred
    #   7. the average duration of most occurred sightings
    #   8. the hour where most sightings occurred
    #   9. most common shape of the sightings
    #   10. most common bigram of the sightings
    colnames(report) <- c("Country", 'Total', "State", "City", "Year", 
                          "Season", "Duration", "Hour", "Shape", "Bigram")
    countries <- list("US", "AU", "DE", "GB", "CA", "UNCLEAR")
    # Row creation for each country
    for (i in (1 : 6)) {
      df.tmp <- df[df$country == countries[i], ]
      state_n_city <- calculation(df.tmp, 5, 0) # Column 3, 4
      freq_year <- calculation(df.tmp, 0, 1)[[1]] # Column 5
      if (freq_year != "Unavailable") freq_year <- strsplit(freq_year, ' ')[[1]][2]
      freq_season <- calculation(df.tmp, 3, 0) # Column 6
      avg_dura <- calculation(df.tmp, 1, 0)[1] # Column 7
      freq_hour <- calculation(df.tmp, 4, 0) # Column 8
      shape_n_bigram <- calculation(df.tmp, 6, 0) # Column 9, 10
      rp_row <- c(countries[i], dim(df.tmp)[1], state_n_city[1], state_n_city[2], freq_year, 
                  freq_season, avg_dura, freq_hour, shape_n_bigram[1], shape_n_bigram[2])
      names(rp_row) <- colnames(report) # Make column names of each row the same as dataframe
      report <- rbind(report, rp_row) # Append row
    }
    # Reorder the rows in decreasing order of total sightings
    report <- report[order(-report$Total), ]
    # Create formatted tabular table, where users can choose to download the output in csv/pdf
    return (report %>% datatable(rownames = F, extensions = "Buttons", 
                                 # Allow hovering on rows
                                 style = 'bootstrap', class = 'table-condensed table-hover',
                                 options = 
                                   list(dom = 'Blfrtip', 
                                        buttons = c('csv', 'pdf'),
                                        # Centering the text in each cell
                                        columnDefs = list(list(className = 'dt-center', 
                                                               targets = '_all')),
                                        # ScrollX for scrolling the table
                                        paging = F, searching = F, autoWidth = F, scrollX = T)))
  }
  
  # Memoise: used for caching plots & table & calculations, so that they will load faster
  memo_spatial <- memoise(spatial_util)
  memo_time <- memoise(time_util)
  memo_desc <- memoise(desc_util)
  memo_calc <- memoise(calc_util)
  memo_report <- memoise(report_generator)
  
  # Set values to reactive variables
  observe({
    # Create a new dataframe based on selected years in slider input
    filterYear <- df %>% filter(between(year, input$B[1], input$B[2]))
    # Use the cached functions to create plots/do calculations
    State$data <- memo_spatial(filterYear, 0, input$A, input$C)
    City$data <- memo_spatial(filterYear, 1, input$A, input$D)
    # If stacked switch is not clicked, then display the plots for individual country
    # along with information boxes on top
    if (!input$switch) {
      Year$data <- memo_time(filterYear, 0, input$A)
      Duration$data <- memo_time(filterYear, 1, input$A)
      Avg_year$data <- memo_calc(filterYear, 0, 0, input$A)
      Max_year$data <- memo_calc(filterYear, 0, 1, input$A)
      Avg_dura$data <- memo_calc(filterYear, 1, 0, input$A)
      Max_dura$data <- memo_calc(filterYear, 1, 1, input$A)
      Min_dura$data <- memo_calc(filterYear, 1, 2, input$A)
      # The outputs for information boxes are put here so that they won't get executed when 
      # user clicks the switch
      output$avgYear <- renderValueBox(
        valueBox(subtitle = p("Yearly Average"), icon = icon("reddit-alien"), color = "blue",
                 Avg_year$data)
      )
      output$maxYear <- renderValueBox(
        valueBox(subtitle = p("Most Sightings occur in ", Max_year$data[1], style = 'font-size:15px;'), icon = icon("reddit-alien"), color = "blue", Max_year$data[2])
      )
      
      output$avgDura <- renderValueBox(
        valueBox(subtitle = "Average Duration", icon = icon("reddit-alien"), color = "blue",
                 paste(Avg_dura$data, "s", sep = ''))
      )
      
      output$maxDura <- renderValueBox(
        valueBox(subtitle = "Max Duration", icon = icon("reddit-alien"), color = "blue",
                 Max_dura$data)
      )
      
      output$minDura <- renderValueBox(
        valueBox(subtitle = "Min Duration", icon = icon("reddit-alien"), color = "blue",
                 Min_dura$data)
      )
    }
    else {
      Year$data <- memo_time(filterYear, 0, 1, 0)
      Duration$data <- memo_time(filterYear, 1, 1, 0)
    }
    
    Season$data <- memo_time(filterYear, 2, input$A)
    Max_season$data <- memo_calc(filterYear, 3, 0, input$A)
    Min_season$data <- memo_calc(filterYear, 3, 1, input$A)
    
    Hour$data <- memo_time(filterYear, 3, input$A)
    Max_hour$data <- memo_calc(filterYear, 4, 0, input$A)
    Min_hour$data <- memo_calc(filterYear, 4, 1, input$A)
    
    Shape$data <- memo_desc(filterYear, 0, input$A)
    WordCloud$data <- memo_desc(filterYear, 1, input$A)
  })
  
  # Predict shape when action button is clicked
  observeEvent(input$Pred, {
    Predshape$data <- 
      paste("most likely shape:",
            shape_classifier(input$predyear, 
                             input$preddura, 
                             input$predseason, 
                             input$predcountry, 
                             input$predday))
  })
  
  # Generate report when action button is clicked
  observeEvent(input$Gen, {
    Report$data <- memo_report(df %>% filter(between(year, input$B[1], input$B[2])))
  })
  # Output the predicted shape
  output$predShape <- renderText(Predshape$data)
  # Output information boxes
  output$topCountry <- renderValueBox(
    valueBox(subtitle = p("Country with most number of sightings is", 
                          memo_calc(df, 2, 0, 1)[1], style = 'font-size:15px;'), 
             icon = icon("reddit-alien"), color = "blue",
             p(memo_calc(df, 2, 0, 1)[2], "in total", style = 'font-size:15px;'))
  )
  output$botCountry <- renderValueBox(
    valueBox(subtitle = p("Country with least number of sightings is", 
                          memo_calc(df, 2, 1, 1)[1], style = 'font-size:15px;'), 
             icon = icon("reddit-alien"), color = "blue",
             p(memo_calc(df, 2, 1, 1)[2], "in total", style = 'font-size:15px;'))
  )
  
  output$maxSeason <- renderValueBox(
    valueBox(subtitle = "Most Frequent Season", icon = icon("reddit-alien"), color = "blue",
             Max_season$data)
  )
  
  output$minSeason <- renderValueBox(
    valueBox(subtitle = "Least Frequent Season", icon = icon("reddit-alien"), color = "blue",
             Min_season$data)
  )
  
  output$maxHour <- renderValueBox(
    valueBox(subtitle = "Most Frequent Observation Hour", 
             icon = icon("reddit-alien"), color = "blue",
             Max_hour$data)
  )
  
  output$minHour <- renderValueBox(
    valueBox(subtitle = "Least Frequent Observation Hour", 
             icon = icon("reddit-alien"), color = "blue",
             Min_hour$data)
  )
  # Render the plots & table and assign them to output variables
  output$pie <- renderggiraph(ggiraph(code = print(pie), bg="transparent"))
  output$map <- renderImage({list(src = "world_map.gif", contentType = 'image/gif')}, 
                            deleteFile = F) # Do not delete the GIF since we need it to be permanent
  output$state <- renderggiraph(State$data)
  output$city <- renderggiraph(City$data)
  output$year <- renderPlot(Year$data, res = 96)
  output$duration <- renderPlot(Duration$data, res = 96)
  output$season <- renderPlot(Season$data, res = 96)
  output$hour <- renderPlot(Hour$data, res = 96)
  output$shape <- renderggiraph(ggiraph(code = print(Shape$data)))
  output$wc <- renderWordcloud2(WordCloud$data)
  output$net <- renderPlot(network, res = 96, bg = "transparent")
  output$chord <- renderPlot(plot_chord(bigram), res = 96, bg = "transparent")
  output$gen_report <- renderDT(Report$data)
}
# Run the shiny application
shinyApp(ui, server)