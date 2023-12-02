library(shiny)
library(tmap)
library(sf)
library(tidymodels)
library(ggplot2)
library(scales)
library(lubridate)

bu_df <- read.csv("bu_df.csv") %>%
  select(-X, -business_id) %>%
  mutate(zip_code = as.factor(postal_code),
         star_bin = as.factor(stars), 
         review_ct_bin = cut(review_count, 
                             breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 10000), 
                             labels = c("<50", "50-100", "100-150","150-200", "200-250", "250-300", "300-350", "350-400", "400+"))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

categories <- bu_df$categories %>% 
  lapply(function(x) unlist(strsplit(x, split = ","))) %>%
  unlist() %>%
  trimws() %>%
  unique() %>%
  sort()

phil_map <- read_sf("philadelphia_census_data.geojson") %>%
  mutate(blw_pct_txt = sprintf("%.2f%%", below_poverty_pct * 100), 
         crime_ct_bin = cut(crime_count, breaks = c(seq(0, 1500, by = 150), 3500)))

hist(phil_map$crime_count, breaks =  c(seq(0, 1500, by = 150), 3500))
trip_df <- read.csv("Trips_by_Distance.csv") %>%
  mutate(
    Date = as.Date(Date, format="%Y/%m/%d"), 
    Year = year(Date), 
    Month = month(Date, label = TRUE),
    Period = ifelse(Date < "2020-01-20", "Pre-COVID", "Mid-COVID"), 
    Population.Staying.at.Home.pct = Population.Staying.at.Home / (Population.Staying.at.Home + Population.Not.Staying.at.Home), 
    Number.of.Trips = Number.of.Trips / 1e6,
    Population.Staying.at.Home = Population.Staying.at.Home / 1e5, 
    Population.Not.Staying.at.Home = Population.Not.Staying.at.Home / 1e5
  )
pre_covid_avg <- trip_df %>% 
  filter(Period == "Pre-COVID") %>% 
  group_by(Month) %>%
  summarize(avg.monthly.trips = sum(Number.of.Trips) / 0.997260273972603)

mid_covid_avg <- trip_df %>% 
  filter(Period == "Mid-COVID") %>% 
  group_by(Month) %>%
  summarize(avg.monthly.trips = sum(Number.of.Trips) / 3.88219178082192)

pre_vs_mid <- full_join(pre_covid_avg, mid_covid_avg, by = "Month", suffix = c(".pre", ".mid")) %>%
  pivot_longer(cols = c("avg.monthly.trips.pre", "avg.monthly.trips.mid")) %>%
  mutate(
    time_period = factor(name, levels = c("avg.monthly.trips.pre", "avg.monthly.trips.mid"), labels = c("Pre-COVID", "Post-COVID")), 
    value = value / 10
  )


# Define UI for application
ui <- fluidPage(
  navbarPage(title = "Philadelphia Restaurants",
    tabPanel("Map",titlePanel("Restaurants Map"),
      sidebarLayout(
          sidebarPanel(
              radioButtons("censusLayerInput", 
                           "Select Visual Layers", 
                           choices = c("Median Income (USD$)", 
                                       "Below Poverty %", 
                                       "Annual Crime")),
              radioButtons("businessDataInput", 
                           "Select business attributes to be shown", 
                           choices = c("Stars", 
                                       "Review Count") # update the list based on what actually matters
                           ), 
              selectInput("categoryInput", 
                          "Select business categories", 
                          choices = categories, 
                          multiple = TRUE)
          ),
          mainPanel(
             tmapOutput("mapPlot", height = 800)
          )
    )), 
    tabPanel("ChatBot Interaction"), 
    tabPanel("COVID Trends", titlePanel("Effects of COVID - Activity"), 
             sidebarPanel(
               dateRangeInput("dateInput", 
                           "Adjust date range:", 
                           min = "2019-01-01", max = "2023-11-18",
                           start = "2019-01-01", end = "2023-11-18",
                           format = "mm/dd/yyyy"
               ), 
               selectInput("tripMetricInput", "Select a Metric:", 
                           choices = c("# of Trips", 
                                       "# of People Staying Home", 
                                       "# of People Not Staying Home",
                                       "Proportion of People Staying Home")), 
               p(strong("Or click buttons to select time period:")),
               actionButton("preCOVIDButton", "Pre-COVID"), 
               actionButton("midCOVIDButton", "During COVID"), 
               actionButton("resetButton", "Reset Dates")),
             mainPanel(
               plotOutput("tripTrendOutput"), 
               plotOutput("tripMonthOutput")
             )
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    observeEvent(input$preCOVIDButton, {
      updateDateRangeInput(inputId = "dateInput", 
                           start = "2019-01-01", 
                           end = "2020-01-20")
    })
    observeEvent(input$midCOVIDButton, {
      updateDateRangeInput(inputId = "dateInput", 
                           start = "2020-01-20", 
                           end = "2023-11-18")
    })
    observeEvent(input$resetButton, {
      updateDateRangeInput(inputId = "dateInput", 
                           start = "2019-01-01", 
                           end = "2023-11-18")
    })
    
    observe({
      # ISSUE WITH OBSERVE
      categories_sub <- input$categoryInput
      contains_all_substrings <- sapply(bu_df$categories, function(string) all(sapply(categories_sub, function(pattern) grepl(pattern, string))))
      
      sub_bu_df <- bu_df %>%
        st_drop_geometry() %>%
        filter(contains_all_substrings)
      
      new_categories <- sub_bu_df %>%
        select(categories) %>%
        lapply(function(x) unlist(strsplit(x, split = ","))) %>%
        unlist() %>%
        trimws() %>%
        unique() %>%
        sort()
      
      updateSelectInput("categoryInput", choices = new_categories)
    })
    
    output$mapPlot <- renderTmap({
      # ID Census layer choice
      layer_choice <- switch(input$censusLayerInput, 
                             "Median Income (USD$)"="mean(median_income)", 
                             "Below Poverty %"="below_poverty_pct" ,
                             "Annual Crime"="crime_ct_bin")
      # ID Restaurant information choice
      rest_info_choice <- switch(input$businessDataInput, 
                             "Stars"="star_bin", 
                             "Review Count"="review_ct_bin")
      rest_palette <- switch(input$businessDataInput, 
                             "Stars"="RdYlGn", 
                             "Review Count"="review_ct_bin")
      tm_basemap(c(BaseMap = "Esri.WorldGrayCanvas"))  +
        tm_shape(phil_map) +
        tm_borders() + 
        tm_fill(col = layer_choice, 
                palette = "Blues", 
                legend.hist = TRUE, 
                alpha = 0.6, 
                popup.vars=c("Annual Crime"="crime_count", 
                             "% Below Poverty Line"="blw_pct_txt", 
                             "Median Income (USD$)"="mean(median_income)"), 
                title = input$censusLayerInput
        ) + 
        tm_layout(legend.outside = TRUE) + 
        tm_shape(bu_df, name = bu_df$name) +
        tm_dots(size = 0.03, 
                col = rest_info_choice, 
                palette = "RdYlGn", 
                popup.vars=c("Avg. Star Rating (Out of 5)"="stars", 
                             "Review Count"="review_count", 
                             "Restaurant Categories"="categories"))# update the list based on what actually matters
    })
    output$tripTrendOutput <- renderPlot({
      trip_df_sub <- trip_df %>%
        filter(Date >= format(input$dateInput[1]), Date <= format(input$dateInput[2]))
      
      metric_selected <- switch(input$tripMetricInput, 
                                "Proportion of People Staying Home" = trip_df_sub$Population.Staying.at.Home.pct, 
                                "# of People Staying Home"=trip_df_sub$Population.Staying.at.Home, 
                                "# of People Not Staying Home"=trip_df_sub$Population.Not.Staying.at.Home, 
                                "# of Trips"=trip_df_sub$Number.of.Trips)
      y_label <- switch(input$tripMetricInput, 
                                "Proportion of People Staying Home" = "Proportion of People Staying HOme", 
                                "# of People Staying Home"="# of People Staying Home [Ten Thousand]", 
                                "# of People Not Staying Home"="# of People Staying Home [Ten Thousand]", 
                                "# of Trips"="Trips [Millions]")
      ggplot(trip_df_sub, aes(Date, metric_selected)) + 
        geom_line() + 
        geom_smooth() + 
        labs(
          x = "Date", 
          y = input$tripMetricInput
        )
    })
    output$tripMonthOutput <- renderPlot({
      ggplot(pre_vs_mid, aes(x = Month, y = value, fill = time_period)) + 
        geom_col(position = "dodge") + 
        labs(x = "", 
             y = "Average Number of Trips Per Month [Millions]", 
             fill = "Time Period"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
