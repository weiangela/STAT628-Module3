# Global
library(shiny)
library(tmap)
library(sf)
library(tidymodels)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(shinyWidgets)
library(DT)
library(plotly)
library(jsonlite)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(syuzhet)
library(stringr)
library(tidytext)
library(udpipe)
library(shinythemes)
library(httr)
# devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
source("sent_shiny.R")

restName <- "¡Juice!"
bu_df <- read.csv("./data/bu_df.csv") %>%
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
restaurants <- sort(unique(bu_df$name))

phil_map <- read_sf("./data/philadelphia_census_data.geojson") %>%
  mutate(blw_pct_txt = sprintf("%.2f%%", below_poverty_pct * 100), 
         crime_ct_bin = cut(crime_count, breaks = c(seq(0, 1500, by = 150), 3500)))

trip_df <- read.csv("./data/Trips_by_Distance.csv") %>%
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


##### Sentiment #####

df_review = read.csv("./data/reviews_with_sentiment.csv")
df_business = stream_in(file("./data/philadelphia_business_info.json"))

df_review = df_review %>% 
  mutate(covid = (date > "2020-01-20 00:00:00"))

ud_model = udpipe_download_model(language = "english", 
                                 model_dir = "udpipe_models")
ud_model = udpipe_load_model(ud_model$file_model)

