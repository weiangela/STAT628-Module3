# UI
source("./tabs/map_tab.R")
source("./tabs/gpt_tab.R")
source("./tabs/covid_tab.R")

# Define UI for application
ui <- fluidPage(
  themeSelector(),
  navbarPage(title = "Philadelphia Restaurants",
             map_tab, 
             gpt_tab, 
             covid_tab
  )
)
