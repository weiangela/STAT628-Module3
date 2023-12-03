bar_subtab = tabPanel("Graphs",
                      fluidRow(
                        column(6, plotlyOutput("PositiveBeforeGraph")), 
                        column(6, plotlyOutput("PositiveAfterGraph"))
                      ),
                      fluidRow(
                        column(6, plotlyOutput("NegativeBeforeGraph")), 
                        column(6, plotlyOutput("NegativeAfterGraph"))
                      ))

wordcloud_subtab = tabPanel("Word Cloud",
                            fluidRow(
                              
                              column(6, 
                                     tags$h3("Postive Sentiment - Pre-COVID"),
                                     wordcloud2Output("PositiveBeforeWC")), 
                              
                              column(6, 
                                     tags$h3("Postive Sentiment - Mid-COVID"),
                                     wordcloud2Output("PositiveAfterWC"))
                            ),
                            fluidRow(
                              
                              column(6, 
                                     tags$h3("Negative Sentiment - Pre-COVID"),
                                     wordcloud2Output("NegativeBeforeWC")), 
                              
                              column(6, 
                                     tags$h3("Negative Sentiment - Mid-COVID"),
                                     wordcloud2Output("NegativeAfterWC"))
                            ))

gpt_subtab = tabPanel("AskGPT",
                      tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
                      tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
                      tags$style(type = "text/css", "label {font-weight: bold;}"),
                      fluidRow(
                        column(12,
                               tags$h3("Chat History"),
                               tags$hr(),
                               # uiOutput("chat_history"),
                               tags$div(style = "overflow-y: scroll; height: 350px;", 
                                        uiOutput("chat_history")),
                               tags$hr())
                      ),
                      fluidRow(
                        column(10,textAreaInput(inputId = "user_message", placeholder = "Enter your message:", label="Your Question", width = "100%")),
                        # tags$p("Click:"),
                        column(1,actionButton("send_message", "Send",
                                              icon = icon("play"),
                                              height = "350px"))
                      ))


gpt_tab = tabPanel("For Owners...", 
                   titlePanel("For Restaurant Owners"), 
                   sidebarPanel(
                     h3("Instructions:"),
                     helpText("This is tab is for restaurant owners to check the result of sentiment analysis, and \
                              get possible feedback from ChatGPT."),
                     helpText("For ", 
                              tags$b(tags$em("Graph")),
                              " subtab, please select your restaurant and the number of words that you are \
                              would like to show on the graphs. The graphs will give most significant words and \
                              their corresponding sentiment score."), 
                     helpText("For ",
                              tags$b(tags$em("Word Cloud")),
                              " subtab, please select your restaurant. The word clouds will show \
                              words with most frequencies."),
                     helpText("For ",
                              tags$b(tags$em("AskGPT")),
                              " subtab, please first type your personal OpenAI apikey, which can be obtained by \
                              the link below. Then, you can ask your own question either by typing and clicking ",
                              tags$b("Send"),
                              " button, or by clicking ",
                              tags$b("AskGPT_Positive"),
                              " or ",
                              tags$b("AskGPT_Negative"),
                              "to ask pre-loaded questions."),
                     
                     # Plots
                     h4("Graph and Word Cloud"),
                     pickerInput("restaurantNameInput", "Select your restaurant!", 
                                 choices = NULL,
                                 multiple = T,
                                 options = list(
                                   `actions-box` = T,
                                   `live-search` = T
                                 )),
                     sliderInput("n_words", "Number of words",
                                 min=5,
                                 max=15,
                                 step=1,
                                 value=10),
                     # GPT
                     h4("ChatGPT"),
                     textInput("api_key", "API Key", "sk-PLACEYOUROWNAPIKEYHERE"),
                     tags$p("Find your own OpenAI API:", 
                            tags$a(href = "https://platform.openai.com/account/api-keys", target="_blank", "https://platform.openai.com/account/api-keys")
                     ),tags$hr(),
                     selectInput("model_name", "Model Name",
                                 choices = c("gpt-4", "gpt-4-0314", 
                                             "gpt-3.5-turbo-0301", 
                                             "gpt-3.5-turbo"), 
                                 selected = "gpt-3.5-turbo"),
                     actionButton("positive_Q", "AskGPT_Positive"),
                     actionButton("negative_Q", "AskGPT_Negative")
                   ), 
                   mainPanel(
                     textOutput("greetings"),
                     dataTableOutput("basicInfoTbl"), 
                     tabsetPanel(
                       bar_subtab,
                       wordcloud_subtab,
                       gpt_subtab
                     )
                   ))
