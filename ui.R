library(shiny)

shinyUI(
    navbarPage(
        
        theme = "bootstrap.css",
        
        title = "Word Prediction",
        
        sidebarLayout(
            
            sidebarPanel(
                
                radioButtons(inputId = "ngram", label = h4("N-Gram Prediction:"),
                             choice=list("Bigram" = "2",
                                         "Trigram" = "3",
                                         "Quadgram" = "4")),
                tags$hr(),  
                
                textInput(inputId = "text", label = h4("Text input"), value = ""),
                submitButton("Update"),
                h4("Next word"),
                textOutput("pred")
            ),
            
            mainPanel(
                
                tabsetPanel(
                    
                    tabPanel(p(icon("list"), "Predictors"),
                             dataTableOutput("pred_table")
                    ),

                    tabPanel(p(icon("list"), "N-Gram"),
                             dataTableOutput("ngram_table")
                    ),
                    
                    tabPanel(p(icon("bar-chart"), "Visualize"),
                             h4("Barplot of the most frequently used terms"),
                             plotOutput("barplot_terms"),
                             h4("Wordcloud of terms"),
                             plotOutput("wordcloud")
                    )
                )
            )
        )
    )
)