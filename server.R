# running with: runApp(display.mode = "showcase")
# debug with:   cat(file = stderr(), paste("value =", val, "\n"))

library(shiny)
library(pryr) # %<a-%
library(wordcloud) # plotting wordcloud

source("prediction.R")
source("plotting.R")

df1 <- read.csv("./data/df1.csv", sep=";")
df2 <- read.csv("./data/df2.csv", sep=";")
df3 <- read.csv("./data/df3.csv", sep=";")
df4 <- read.csv("./data/df4.csv", sep=";")

shinyServer(function(input, output, session) {
    
    ngram <- reactive({
        
        switch(input$ngram,
               "2" = {df <- df2},
               "3" = {df <- df3},
               "4" = {df <- df4})
        return(df)
        
    })
    
    pred <- reactive({
        
        switch(input$ngram,
               "2" = {pred <- pred_ngram(input = input$text,
                                         df_min = df1,
                                         df_max = df2,
                                         n = 2)},
               "3" = {pred <- pred_ngram(input = input$text,
                                        df_min = df2,
                                        df_max = df3,
                                        n = 3)},
               "4" = {pred <- pred_ngram(input = input$text,
                                        df_min = df3,
                                        df_max = df4,
                                        n = 4)})

        if(is.null(pred)) {pred <- data.frame(word = character(),
                                              count = numeric(),
                                              prob = numeric())}
        return(pred)
               
    })

    output$wordcloud <- renderPlot({
        
        a %<a-% suppressWarnings(
            wordcloud(ngram()$terms, ngram()$count, max.words = 30,
                          colors = rev(brewer.pal(10, "RdYlBu")),
                          random.order = FALSE))
        return(a)
    })
    
    output$barplot_terms <- renderPlot({
        g <- barplot_terms(ngram())
        return(g)
    })
    
    output$ngram_table <- renderDataTable({
        return(ngram())
    })
    
    output$pred_table <- renderDataTable({
        return(pred())
    })
    
    output$pred <- renderText({
        if(nrow(pred()) == 0) {return("no prediction")}
        return(pred()$word[1])
 
    })
})
