map_tab = tabPanel("Map",titlePanel("Restaurants Map"),
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
             )
           ),
           mainPanel(
             tmapOutput("mapPlot", height = 800)
           )
         ))
