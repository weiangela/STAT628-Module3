
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Splash Page
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
  
  # observe({
  #   # ISSUE WITH OBSERVE
  #   categories_sub <- input$categoryInput
  #   contains_all_substrings <- sapply(bu_df$categories, function(string) all(sapply(categories_sub, function(pattern) grepl(pattern, string))))
  #   
  #   sub_bu_df <- bu_df %>%
  #     st_drop_geometry() %>%
  #     filter(contains_all_substrings)
  #   
  #   new_categories <- sub_bu_df %>%
  #     select(categories) %>%
  #     lapply(function(x) unlist(strsplit(x, split = ","))) %>%
  #     unlist() %>%
  #     trimws() %>%
  #     unique() %>%
  #     sort()
  #   
  #   updateSelectInput("categoryInput", choices = new_categories)
  # })
  
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
  
  ####### Restaurant owner page#####
  updatePickerInput(session, 'restaurantNameInput', 
                    choices = df_business %>% 
                      filter(is_open == 1) %>% 
                      pull(name))
  
  restName <- reactive({input$restaurantNameInput})
  # output$greetings <- renderText({
  #   paste("Hello, ", restName())
  # })
  # 
  output$density_sent = renderPlot({
    plot_density(input$restaurantNameInput, "")
  })
  
  output$basicInfoTbl <- renderDataTable(
    datatable(
      bu_df %>%
        select(name, postal_code, stars, review_count, categories, video, dj, NoiseLevel, dessert, street, valet, DogsAllowed, classy, touristy) %>% 
        filter(name %in% restName()),
      filter = "top",
      rownames = F,
      options = list(# scrollY = 100,
                     scrollX = 500,
                     deferRender = TRUE,
                     # pageLength = 10,
                     autoWidth = T
      )
    )
  )
  
  ##### Graphs #####
  output$PositiveBeforeGraph = renderPlotly({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, before covid
      temp = get_temp(input$restaurantNameInput, "", T, F)
      plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words, color = "springgreen3")
      ggplotly(plot_temp$plot + 
                 ggtitle("Postive Words Before COVID"))
    }
  })
  output$PositiveAfterGraph = renderPlotly({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, after covid
      temp = get_temp(input$restaurantNameInput, "", T, T)
      plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words, color = "tomato")
      ggplotly(plot_temp$plot + 
                 ggtitle("Postive Words During COVID"))
    }
  })
  output$NegativeBeforeGraph = renderPlotly({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Negative, before covid
      temp = get_temp(input$restaurantNameInput, "", F, F)
      plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words, color = "royalblue")
      ggplotly(plot_temp$plot + 
                 ggtitle("Negative Words Before COVID"))
    }
  })
  output$NegativeAfterGraph = renderPlotly({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, before covid
      temp = get_temp(input$restaurantNameInput, "", F, T)
      plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words, color = "lightcoral")
      ggplotly(plot_temp$plot + 
                 ggtitle("Negative Words During COVID"))
    }
  })
  ##### Word Cloud #####
  output$PositiveBeforeWC = renderWordcloud2({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, before covid
      temp = get_temp(input$restaurantNameInput, "", T, F)
      
      plot_word_cloud(temp$Pivot_df)
    }
  })
  output$PositiveAfterWC = renderWordcloud2({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, after covid
      temp = get_temp(input$restaurantNameInput, "", T, T)

      plot_word_cloud(temp$Pivot_df)
    }
  })
  output$NegativeBeforeWC = renderWordcloud2({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Negative, before covid
      temp = get_temp(input$restaurantNameInput, "", F, F)
      plot_word_cloud(temp$Pivot_df)
    }
  })
  output$NegativeAfterWC = renderWordcloud2({
    if (length(input$restaurantNameInput) == 0){
      NULL
    }else{
      # Positive, before covid
      temp = get_temp(input$restaurantNameInput, "", F, T)
      plot_word_cloud(temp$Pivot_df)
    }
  })
  
  ##### GPT #####
  chat_data <- reactiveVal(data.frame())
  
  observeEvent(input$send_message, {
    if (input$user_message != "") {
      new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))
      
      gpt_res <- call_gpt_api(input$api_key, input$user_message, input$model_name)
      
      if (!is.null(gpt_res)) {
        gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
      }
      updateTextInput(session, "user_message", value = "")
    }
  })
  
  call_gpt_api <- function(api_key, prompt, model_name, temperature=1, max_length=2048, sysprompt="") {
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = model_name,
        messages = list(
          list(role = "user", content = prompt),
          list(role = "system", content = sysprompt)
        ),
        temperature = temperature,
        max_tokens = max_length
      )
    )
    return(str_trim(content(response)$choices[[1]]$message$content))
  }
  
  observeEvent(input$positive_Q, {
    temp = get_temp(input$restaurantNameInput, "", T, F)
    plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words)
    temp_1 = get_temp(input$restaurantNameInput, "", T, T)
    plot_temp_1 = plot_bar(temp_1$Full_df, temp_1$Pivot_df, n=input$n_words)
    text = paste0("I have two data frames, containing positive key words, sentiment scores, and frequency of a restaurant before and during covid time. Before covid time, the words are: ",
                  paste(plot_temp$score$word, collapse = ", "), ". And the corresponding scores are ",
                  paste(plot_temp$score$scores, collapse = ", "), ". And the frequencies are ",
                  paste(plot_temp$score$freq, collapse = ", "), ". During covid time, the words are ",
                  paste(plot_temp_1$score$word, collapse = ", "), ". And the corresponding scores are ",
                  paste(plot_temp_1$score$scores, collapse = ", "), ". And the frequencies are ",
                  paste(plot_temp_1$score$freq, collapse = ", "),
                  ". Can you tell me if there are any differences between those two data set? If there is, what should I pay attention to as a restaurant owner?")

    new_data <- data.frame(source = "User", message = text, stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), new_data))
    
    gpt_res <- call_gpt_api(input$api_key, text, input$model_name)
    
    if (!is.null(gpt_res)) {
      gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), gpt_data))
    }
    updateTextInput(session, "user_message", value = "")
    
  })
  
  observeEvent(input$negative_Q, {
    temp = get_temp(input$restaurantNameInput, "", F, F)
    plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=input$n_words)
    temp_1 = get_temp(input$restaurantNameInput, "", F, T)
    plot_temp_1 = plot_bar(temp_1$Full_df, temp_1$Pivot_df, n=input$n_words)
    text = paste0("I have two data frames, containing negative key words, sentiment scores, and frequency of a restaurant before and during covid time. Before covid time, the words are: ",
                  paste(plot_temp$score$word, collapse = ", "), ". And the corresponding scores are ",
                  paste(plot_temp$score$scores, collapse = ", "), ". And the frequencies are ",
                  paste(plot_temp$score$freq, collapse = ", "), ". During covid time, the words are ",
                  paste(plot_temp_1$score$word, collapse = ", "), ". And the corresponding scores are ",
                  paste(plot_temp_1$score$scores, collapse = ", "), ". And the frequencies are ",
                  paste(plot_temp_1$score$freq, collapse = ", "),
                  ". Can you tell me if there are any differences between those two data set? If there is, what should I pay attention to as a restaurant owner?")
    
    new_data <- data.frame(source = "User", message = text, stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), new_data))
    
    gpt_res <- call_gpt_api(input$api_key, text, input$model_name)
    
    if (!is.null(gpt_res)) {
      gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), gpt_data))
    }
    updateTextInput(session, "user_message", value = "")
    
  })
  
  
  output$chat_history <- renderUI({
    chatBox <- lapply(1:nrow(chat_data()), function(i) {
      tags$div(class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
               HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"])))
    })
    do.call(tagList, chatBox)
  })
  
  # COVID Trips Page
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
  
  ########## UPDATE WITH PLOTLY #########
  output$tripMonthOutput <- renderPlot({
    ggplot(pre_vs_mid, aes(x = Month, y = value, fill = time_period)) + 
      geom_col(position = "dodge") + 
      labs(x = "", 
           y = "Average Number of Trips Per Month [Millions]", 
           fill = "Time Period"
      )
  })
}
