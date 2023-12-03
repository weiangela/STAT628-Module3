

covid_tab = tabPanel("COVID Trends", titlePanel("Effects of COVID - Activity"), 
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
                       plotlyOutput("tripTrendOutput"), 
                       plotlyOutput("tripMonthOutput")
                     )
)
