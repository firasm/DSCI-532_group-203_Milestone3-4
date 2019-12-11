### Extending app3.R and filling in the blanks
### Adding full interactivity

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(sf)
library(ggplot2)
library(dplyr)
library(gghighlight)


app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Load Map data
cp_map <- st_read("./data/central_park_geo.geojson", quiet = TRUE)

# Update values for different zones of CP West
cp_map$location <- as.character(cp_map$location)
cp_map$sitename <- as.character(cp_map$sitename)
cp_map$sitename[cp_map$location == 'CPW, W 97 St, West Drive, W 100 St'] <- 'Central Park West (Zone 1)'
cp_map$sitename[cp_map$location == 'CPW, 85 St Transverse, West Drive To 96 St'] <- 'Central Park West (Zone 2)'
cp_map$sitename[cp_map$location == 'West Drive, CPW, 65 St Transverse'] <- 'Central Park West (Zone 3)'
cp_map$sitename[cp_map$location == '66 St To 72 St, CPW To West Drive'] <- 'Central Park West (Zone 4)'

# Add numbers for each zone to display on map 
cp_map$number <- seq(1:63)

# Read in squirrel data
squirrel_data <- read.csv('./data/squirrel_data.csv')

# Filter data for counts about 5 squirrels
gt_5_sq <- squirrel_data %>% 
  select(sitename, Unique_Squirrel_ID) %>% 
  filter(Unique_Squirrel_ID > 5) %>% 
  pull(sitename)

# Find center of each polygon to plot region number
centroids_filtered <- cp_map %>%
    filter((sitename %in% gt_5_sq)) %>%
    st_centroid() %>% 
    bind_cols(as_data_frame(st_coordinates(.)))

# Update positions of centroids on map
centroids_filtered$X[centroids_filtered$number == 52] <- -73.9581129527627
centroids_filtered$Y[centroids_filtered$number == 52] <- 40.7998257114188
centroids_filtered$X[centroids_filtered$number == 23] <- -73.973088868377
centroids_filtered$Y[centroids_filtered$number == 23] <- 40.7787970822199
centroids_filtered$X[centroids_filtered$number == 62] <- -73.9643990164278
centroids_filtered$X[centroids_filtered$number == 26] <- -73.9664505849341
centroids_filtered$X[centroids_filtered$number == 63] <- -73.9512081624995
centroids_filtered$Y[centroids_filtered$number == 51] <- 40.7698840126662
centroids_filtered$X[centroids_filtered$number == 53] <- -73.9690122680823

#Join map boundaries to squirrel data
data_map <- left_join(cp_map %>% 
  filter(sitename %in% gt_5_sq), squirrel_data, by = 'sitename')

# Change certain colnames
colnames(data_map) <- c('propname',
'us_congres',
'zipcode',
'acres',
'location',
'gispropnum',
'retired',
'subcategor',
'communityb',
'department',
'precinct',
'retireddat',
'omppropid',
'nys_assemb',
'sitename',
'nys_senate',
'councildis',
'borough',
'descriptio',
'number',
'X',
'Unique_Squirrel_ID',
'Climbing',
'Approaching_humans',
'Vocalizing',
'Running_or_chasing',
'Eating_or_foraging',
'Count_diff',
'geometry')

data_map <- data_map %>% 
    mutate(am_pm = case_when(Count_diff < 0 ~ "Higher count in PM", 
                            TRUE ~ "Higher count in AM"))

# Selection components

label_names = paste0(data_map$number,'. ', data_map$sitename)

regionDropdown <- dccDropdown(
  id = "sitename",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map2(data_map$sitename,label_names,function(x,y){list(label=y, value=x)}),
  value = as.factor(levels(data_map$sitename)), #Selects all by default
  multi = TRUE
)

behaviorDropdown <- dccDropdown(
  id = "behavior",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = list(list(label ='Running or chasing',value = 'Running_or_chasing'),
  list(label ='Climbing',value ='Climbing'),
  list(label ='Eating or foraging',value ='Eating_or_foraging'),
  list(label ='Vocalizing', value ='Vocalizing')),
  value = 'Running_or_chasing'
)

# Graphing Functions

#' Plots a choropleth of central park filled with color
#' according to count of squirrels. 
#'
#' @param highlight (optional) a character vector of sitenames to highlight
#'
#' @return ggplot chart
#'
#' @examples
#' plot_choropleth(c('Ross Pinetum', 'The Ramble'))
plot_choropleth <- function(highlight = vector()) {
    full_map <- ggplot(data_map) + 
        geom_sf(aes(fill = Unique_Squirrel_ID), color = 'lightgrey') + 
        geom_text(aes(X, Y, label = number), 
                  data = centroids_filtered, 
                  size = 3, 
                  color = 'black') +        
        scale_fill_gradient(low = 'white', high = 'darkgreen', 
                            limits = c(0,max(squirrel_data$Unique_Squirrel_ID)),
                            name = 'Count') +
        labs(title = 'Central Park Squirrel Distribution', x = '', y = '') +
        theme_minimal() +
        theme(legend.position = c(0.8, 0.2), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))
    if (length(highlight) == 0) {
        ggplotly(full_map)
    } else {
        m_h <- full_map + 
        gghighlight(sitename %in% highlight, 
                    label_key = Unique_Squirrel_ID)
        ggplotly(m_h)
    }
}

#' Plots a bar chart of squirrel count by park region
#'
#' @param highlight (optional) a character vector of sitenames to highlight
#'
#' @return ggplot chart
#'
#' @examples
#' plot_counts_bar(c('Ross Pinetum', 'The Ramble'))
plot_counts_bar <- function(highlight = vector()) {
    counts_full <- ggplot(data_map) + 
            geom_bar(aes(x = reorder(sitename, Unique_Squirrel_ID), 
                         y = Unique_Squirrel_ID, 
                         fill = Unique_Squirrel_ID, text = paste("Area name:", sitename, '<br>',"Squirrel count:", Unique_Squirrel_ID)), 
                     stat = 'identity') + 

            coord_flip() +
            scale_fill_gradient(low = 'white', 
                                high = 'darkgreen', 
                                limits = c(0,max(squirrel_data$Unique_Squirrel_ID)),
                                name = 'Count') +
            labs(title = 'Squirrel Distribution by Park Region', y = 'Squirrel Count', x = '') +
            theme_minimal() +
            theme(panel.grid.major.y = element_blank(), legend.position = c(0.8, 0.2), plot.title = element_text(hjust = 0.5))
    if (length(highlight) == 0) {
        ggplotly(counts_full, tooltip="text")
        } else {
            c_h <- counts_full +
                 gghighlight(sitename %in% highlight, 
                             label_key = Unique_Squirrel_ID)
            ggplotly(c_h,tooltip="text")
    }
}

# Use a function make_graph() to create the graph

#' Plots a bar chart of change in squirrel count from AM to PM
#'    by park region
#'
#' @param highlight (optional) a character vector of sitenames to highlight
#'
#' @return ggplot chart
#'
#' @examples
#' plot_diff_bar(c('Ross Pinetum', 'The Ramble'))
plot_diff_bar <- function(highlight = list()) {
    diff_bar <- ggplot(data_map,
               aes(x = reorder(sitename, -Count_diff), 
                   y = -Count_diff,
                   fill = am_pm, text = paste("Area name:", sitename, '<br>',"Count difference:", -Count_diff))) +
        geom_bar(stat = "identity")  +
        labs(title = 'Squirrel Abundance: Morning vs. Afternoon', 
             y = 'Difference in count', 
             x = '') +
        scale_y_continuous(breaks=seq(-20,50,10)) +
        coord_flip() +
        theme_minimal() +
        theme(panel.grid.major.y = element_blank(), 
              legend.position = c(0.8, 0.1), 
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank()) 

    if (length(highlight) == 0){
      ggplotly(diff_bar, tooltip="text")
    } else {
        db_h <- diff_bar + gghighlight(sitename %in% highlight, 
            label_key = Unique_Squirrel_ID)
        ggplotly(db_h, tooltip="text")
    }

}

#' Plots a bar chart of squirrel behavior by park region
#'
#' @param highlight (default = empty) a character vector of sitenames to highlight
#' @param behavior (default = Running_or_chasing) column of behavior to plot with options
#'       Running_or_chasing, Eating_or_foraging, Vocalizing, Approaching_humans, Climbing
#'
#' @return ggplot chart
#'
#' @examples
#' plot_behaviors_bar(c('Ross Pinetum', 'The Ramble'))
plot_behaviors_bar <- function(behavior = 'Running_or_chasing', highlight = vector()) {
    b_bar <- ggplot(data_map) + 
        geom_bar(aes(reorder(sitename, !!sym(behavior)), !!sym(behavior),text = paste("Area name:", sitename, '<br>',"Squirrel count:", !!sym(behavior))),
                     stat = 'identity') + 
        coord_flip() +
        labs(title = paste('Count of Squirrels',
                           str_replace_all(behavior,'_',' '),
                           ' by Park Region'), 
             y = 'Squirrel Count', x = '') +
        theme_minimal() +
        theme(panel.grid.major.y = element_blank(), 
              plot.title = element_text(hjust = 0.5))
    if (length(highlight) == 0) {
        ggplotly(b_bar, tooltip="text")
    } else {
        b_h <- b_bar + gghighlight(sitename %in% highlight, 
            label_key = Unique_Squirrel_ID)
        ggplotly(b_h,tooltip="text")
    }
}

# Now we define the graph as a dash component using generated figure
map_graph <- dccGraph(
  id = 'map-graph',
  figure = plot_choropleth(),
  style = list(height = 900, center = TRUE)
)

count_graph <- dccGraph(
  id = 'count-graph',
  figure=plot_counts_bar(), # gets initial data using argument defaults
  style = list(height = 800, center = TRUE)
)

countdiff_graph <- dccGraph(
  id = 'count-diff-graph',
  figure=plot_diff_bar(), # gets initial data using argument defaults
  style = list(height = 800, center = TRUE)
)

b_graph <- dccGraph(
  id = 'b-graph',
  figure=plot_behaviors_bar(), # gets initial data using argument defaults
  style = list(height = 800, center = TRUE)
)

# Define layout components

colors <- list(
  background = '#C4BFB9',
  text = 'black'
)
pageTitle <- htmlH1(
  'Welcome to the Squirrel Park App!',
  style = list(
    textAlign = 'center',
    color = colors$text
  )
)

pageSubTitle <- htmlDiv(
  "Guide your observance of the famous squirrels of New York's Central Park",
  style = list(
    textAlign = 'center',
    color = colors$text
  )
)

app_description <- htmlDiv(
  "View squirrel distribution by park region, time of the day, and behavior.",
  style = list(
    textAlign = 'center',
    color = colors$text
  )
)


app$layout(
  htmlDiv(
    list(
      # space
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      pageTitle,
      pageSubTitle,
      app_description,
      #selection components
      htmlIframe(height=50, width=10, style=list(borderWidth = 0)), #space
      htmlLabel('Select park region:'),
      htmlLabel('The selected park regions will be highlighted in all the plots. Click "x" in front of the labels to cancel selection. You may also cancel all your selections by clicking the "x" at the end of the drop-down menu.'),
      regionDropdown,
      map_graph,
      #space
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      count_graph,
      #space
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      htmlLabel('In the following plot, blue bar means more squirrels in the afternoon (PM). Red bar means more in the morning (AM).'),
      countdiff_graph,
      #space
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      htmlLabel('Select behavior to display below:'),
      behaviorDropdown,
      b_graph,
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
      dccMarkdown("[Data Source](https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw)")
    )
 #   style = list(backgroundColor = colors$background)
  )
)

# Adding callbacks for interactivity
# We need separate callbacks to update graph and table
# BUT can use multiple inputs for each!

app$callback(
  #update figure of gap-graph
  output=list(id = 'count-diff-graph', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'sitename', property='value')),
  #this translates your list of params into function arguments
  function(highlight_list) {
    plot_diff_bar(highlight = highlight_list)
  })

app$callback(
  output=list(id = 'b-graph', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'behavior', property='value'),
              input(id = 'sitename', property='value')),
  #this translates your list of params into function arguments
  function(behavior_val, highlight_list) {
    plot_behaviors_bar(behavior = behavior_val, highlight = highlight_list)
  })

app$callback(
  #update figure of gap-graph
  output=list(id = 'count-graph', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'sitename', property='value')),
  #this translates your list of params into function arguments
  function(highlight_list) {
    plot_counts_bar(highlight = highlight_list)
  })

  app$callback(
  #update figure of gap-graph
  output=list(id = 'map-graph', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'sitename', property='value')),
  #this translates your list of params into function arguments
  function(highlight_list) {
    plot_choropleth(highlight = highlight_list)
  })



app$run_server()