# Word Cloud
get_temp = 
  function(business_name, business_type, positive = T, in_covid = T){
    buis_open = df_business %>% 
      filter(# is_open == 1,
             grepl(paste(business_type, collapse = "|"),
                   tolower(categories)),
             name %in% business_name) %>% 
      pull(business_id) %>% 
      unique()

    # filter data
    df_review_during = df_review %>% 
      filter(covid == in_covid,
             business_id %in% buis_open) %>% 
      select(review_id, business_id, stars, text, sentiment, covid)
      # group_by(business_id) %>% 
      # mutate(n = n()) %>% 
      # filter(n >= 50) %>% 
      # ungroup()
    
    # split the sentence into words
    df_review_during_words = df_review_during %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words, by = "word") %>% 
      mutate(sent_word = get_sentiment(word, method = "afinn"))
    
    # words before and after a word
    context_words = df_review_during_words %>%
      group_by(review_id) %>%
      arrange(review_id, row_number()) %>%
      mutate(before_word = lag(word, 1),
             after_word = lead(word, 1)) %>% 
      ungroup()
    
    if (positive){
      context_words_filtered = context_words %>% 
        filter(sent_word > 0)
    }else{
      context_words_filtered = context_words %>% 
        filter(sent_word < 0)
    }
    
    context_words_pivot = 
      c(context_words_filtered$before_word, 
        context_words_filtered$after_word) %>% 
      na.omit() %>%
      table() %>% 
      as.data.frame()
      
    colnames(context_words_pivot) = c("word", "freq")
    
    type_df = as.data.frame(udpipe_annotate(ud_model, x = context_words_pivot$word))
    context_words_pivot = context_words_pivot %>% 
      mutate(
        type = type_df$upos[!duplicated(type_df$doc_id)]
        )
    
    return(list(
      "Full_df" = context_words_filtered,
      "Pivot_df" = context_words_pivot))
  }


plot_word_cloud = function(df){
    # Plot word cloud
    set.seed(7)
    # with(df %>%
    #        filter(type == "NOUN"),
    #      wordcloud(words = word, freq = freq,
    #                max.words=20, random.order=FALSE, rot.per=0.35,
    #                colors=brewer.pal(8, "Dark2")))
    wordcloud2(df %>% filter(type == "NOUN") %>% arrange(-freq) %>% head(30),
               size=.5, color='random-dark')
    # wordcloud2(data = dff, size=.5, color='random-dark')
}

plot_bar = function(df_full, df_pivot, n = 10, color = "royalblue"){
  words = 
    df_pivot %>% 
    filter(type == "NOUN") %>% 
    arrange(-freq) %>% 
    head(n) %>% 
    pull(word)
  
  scores = c()
  for (i in 1:length(words)){
    score = df_full %>% 
      filter(before_word == words[i] | 
               after_word == words[i]) %>% 
      summarise(score = sum(sent_word)) %>% 
      pull(score)
    scores = c(scores, score)
  }
  
  scores = scores / length(unique(df_full$review_id))
  score_df = 
    data.frame(words, scores) %>% 
    arrange(-scores)
  colnames(score_df)[1] = "word"
  p = ggplot(score_df, 
             aes(y = reorder(words, scores), x=scores)) + 
    geom_bar(stat = "identity", fill = color) + 
    labs(x = "", y = "")
  
  score_df = score_df %>% 
    left_join(df_pivot, by = "word")
  
  return(list("plot" = p,
              "score" = score_df))
}


# Positive, before covid
# temp = get_temp("The Twisted Tail", "", T, F)
# plot_temp = plot_bar(temp$Full_df, temp$Pivot_df, n=10)
# plot_temp$plot
# # Positive, during covid
# temp_1 = get_temp("The Twisted Tail", "", T, T)
# plot_temp_1 = plot_bar(temp_1$Full_df, temp_1$Pivot_df, color = "orange")
# plot_temp_1$plot
# 
# source("./NB.R")
# text = paste0("I have two data frames, containing words and sentiment scores of a restaurant before and during covid time. Before covid time, the words are: ",
#               paste(plot_temp$score$words, collapse = ", "), " And the corresponding scores are ",
#               paste(plot_temp$score$scores, collapse = ", "), "During covid time, the words are ",
#               paste(plot_temp_1$score$words, collapse = ", "), " And the corresponding scores are ",
#               paste(plot_temp_1$score$scores, collapse = ", "), ". Can you tell me if there are any differences between those two data set? If there is, what should I pay attention to as a restaurant holder?")
# answer = NB(text)
# 
# cat(answer)

