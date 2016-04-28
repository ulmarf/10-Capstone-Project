library(dplyr) # mutate, filter, arrange, select,
library(tm) # VCorpus, tm_map
library(RWeka) # n-gram-model

library(ggplot2)
library(RColorBrewer)
library(wordcloud) # plotting wordcloud
library(pryr) # %<a-%

if(!file.exists("Swiftkey.zip")){
    url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url, "Swiftkey.zip")
}
if(!file.exists("./final/en_US/en_US.blogs.txt")){
    unzip(zipfile = "Swiftkey.zip")
}

if(!(exists("twitter"))) {
    con <- file("./final/en_US/en_US.twitter.txt", "rb")
    twitter <- readLines(con, encoding = "UTF-8", warn = F)
    close(con)
}
if(!(exists("news"))) {
    con <- file("./final/en_US/en_US.news.txt", "rb")
    news <- readLines(con, encoding = "UTF-8", warn = F)
    close(con)  
}
if(!(exists("blogs"))) {
    con <- file("./final/en_US/en_US.blogs.txt", "rb")
    blogs <- readLines(con, encoding = "UTF-8", warn = F)
    close(con)
}

if(!file.exists("en_US.RData")) {
    save(blogs, news, twitter, file = "en_US.RData")
}
    
if(exists("twitter")) {remove(twitter)}
if(exists("news")) {remove(news)}
if(exists("blogs")) {remove(blogs)}

gc() # garbage collection

################################################################################
# Building n-grams

if(!file.exists("./data/df1.csv")) {
    
    gc()
    
    load("en_US.RData")
    
    data_print <- data.frame(
        source = c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"),
        MB = signif(c(file.info("./final/en_US/en_US.blogs.txt")$size,
                 file.info("./final/en_US/en_US.news.txt")$size,
                 file.info("./final/en_US/en_US.twitter.txt")$size) / 1024 ^ 2, 6),
        length = c(length(blogs), length(news), length(twitter)),
        maxChar = c(max(nchar(blogs)), max(nchar(news)), max(nchar(twitter)))
    )
    print(data_print)
    
    set.seed(333)
    
    n_sample <- 10000
    
    data <- c(
        twitter[sample(1:length(twitter), size = n_sample, replace = F)],
        news[sample(1:length(news), size = n_sample, replace = F)],
        blogs[sample(1:length(blogs), size = n_sample, replace = F)])
 
    data_text <- paste(data, collapse = " ")
    
    expand_contractions <- function(data_text){
        data_text <- gsub("won't", "will not", data_text)
        data_text <- gsub("n't", " not", data_text)
        data_text <- gsub("'d", " would", data_text)
        data_text <- gsub("'ll", " will", data_text)
        data_text <- gsub("'m", " am", data_text)
        data_text <- gsub("'re", " are", data_text)
        data_text <- gsub("'ve", " have", data_text)
        data_text <- gsub("'s", "", data_text)
    }
    
    data_text <- expand_contractions(data_text)
    
    vcorpus <- VCorpus(VectorSource(data_text),
                      readerControl = list(reader = readPlain,
                                      language = "english",
                                      encoding = "ANSI"))
    vcorpus <- tm_map(vcorpus, stripWhitespace)
    vcorpus <- tm_map(vcorpus, removePunctuation)
    vcorpus <- tm_map(vcorpus, content_transformer(tolower))
    vcorpus <- tm_map(vcorpus, content_transformer(removeNumbers))
    # source: https://code.google.com/archive/p/badwordslist/downloads
    vorcpus <- tm_map(vcorpus, removeWords, "./badwords.txt")
    # Stemming Words: "example" and "examples" are both stemmed to "exampl"
    # vcorpus <- tm_map(vcorpus, stemDocument)
    
    ngram1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
    ngram2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    ngram3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    ngram4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
    
    tdm1 <- TermDocumentMatrix(vcorpus, control = list(tokenize = ngram1))
    tdm2 <- TermDocumentMatrix(vcorpus, control = list(tokenize = ngram2))
    tdm3 <- TermDocumentMatrix(vcorpus, control = list(tokenize = ngram3))
    tdm4 <- TermDocumentMatrix(vcorpus, control = list(tokenize = ngram4))
    
    str(tdm2)
    # List of 6
    # $ i       : int [1:111046] 1 2 3 4 5 6 7 8 9 10 ...
    # $ j       : int [1:111046] 1 1 1 1 1 1 1 1 1 1 ...
    # $ v       : num [1:111046] 1 1 3 1 1 1 1 3 1 1 ...
    # $ nrow    : int 111046
    # $ ncol    : int 1
    # $ dimnames:List of 2
    # ..$ Terms: chr [1:111046] "'s seductively""| __truncated__ "'s top""| __truncated__ "– a""| __truncated__ "– add""| __truncated__ ...
    # ..$ Docs : chr "1"
    # - attr(*, "class")= chr [1:2] "TermDocumentMatrix" "simple_triplet_matrix"
    # - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
    head(tdm4$dimnames$Terms, 10) # terms
    head(tdm4$v, 10) # count
    
    mat1 <- as.matrix(tdm1)
    mat2 <- as.matrix(tdm2)
    mat3 <- as.matrix(tdm3)
    mat4 <- as.matrix(tdm4)
    
    freq1 <- sort(rowSums(mat1), decreasing = T)
    freq2 <- sort(rowSums(mat2), decreasing = T)
    freq3 <- sort(rowSums(mat3), decreasing = T)
    freq4 <- sort(rowSums(mat4), decreasing = T)
    
    head(freq2, 5)
    # of the  in the  to the  on the for the 
    #    850     766     370     368     326 
    
    df_ngram <- function(tdm) {
        df <- as.data.frame(as.matrix(tdm))
        df <- df %>%
            mutate(terms = row.names(df),
                   count = rowSums(df)) %>%
            # filter(count > 1) %>%
            arrange(desc(count)) %>%
            select(terms:count)
        df <- df %>% mutate(prob = count / sum(df$count))
        return(df)
    }
    
    df1 <- df_ngram(tdm1)
    df2 <- df_ngram(tdm2)
    df3 <- df_ngram(tdm3)
    df4 <- df_ngram(tdm4)
    
    # save data
    write.table(df1, "./data/df1.csv", sep = ";", col.names = T, row.names = F)
    write.table(df2, "./data/df2.csv", sep = ";", col.names = T, row.names = F)
    write.table(df3, "./data/df3.csv", sep = ";", col.names = T, row.names = F)
    write.table(df4, "./data/df4.csv", sep = ";", col.names = T, row.names = F)
    
}

if(!(exists("df1"))) {df1 <- read.csv("./data/df1.csv", sep=";")}
if(!(exists("df2"))) {df2 <- read.csv("./data/df2.csv", sep=";")}
if(!(exists("df3"))) {df3 <- read.csv("./data/df3.csv", sep=";")}
if(!(exists("df4"))) {df4 <- read.csv("./data/df4.csv", sep=";")}

# convert factors into characters
df1$terms <- as.character(df1$terms)
df2$terms <- as.character(df2$terms)
df3$terms <- as.character(df3$terms)
df4$terms <- as.character(df4$terms)

# barplot for frequencies
barplot_terms <- function(df) {
    g <- ggplot(data = head(arrange(df, desc(count)), 10),
                aes(x = reorder(terms, desc(count)), y = count))
    g <- g + geom_bar(stat = "identity", fill = "darkblue")
    g <- g + labs(x = "words", y = "frequency",
                  title = "barplot of word frequencies")
    g <- g + theme_bw()
    # g <- g + theme(axis.text.x = element_text(angle = 90, size = 10))
    return(g)
}

g1 <- barplot_terms(df1); # print(g1)
g2 <- barplot_terms(df2); # print(g2)
g3 <- barplot_terms(df3); # print(g3)
g4 <- barplot_terms(df4); # print(g4)

# wordclouds
w2 %<a-% wordcloud(df2$terms, df2$count, max.words = 30,
                   colors = rev(brewer.pal(10, "RdYlBu")),
                   random.order = FALSE,
                   main = "Title")

w3 %<a-% wordcloud(df3$terms, df3$count, max.words = 30,
                   colors = rev(brewer.pal(10, "RdYlBu")),
                   random.order = FALSE,
                   main = "Title")
par(mfrow = c(1, 2))
w2
print(w3)
par(mfrow = c(1, 1))

data_print <- data.frame(
    "percent" = c("0.5", "0.9", "0.95"),
    "number" = c(
        row.names(df1[min(which((cumsum(df1$count) / sum(df1$count)) >= 0.5)),]),
        row.names(df1[min(which((cumsum(df1$count) / sum(df1$count)) >= 0.9)),]),
        row.names(df1[min(which((cumsum(df1$count) / sum(df1$count)) >= 0.95)),]))
)

data_plot <- df1
data_plot$index <- 1:nrow(data_plot)
data_plot <- within(data_plot, acc_sum <- cumsum(count))
data_plot$perc <- data_plot$acc_sum / sum(data_plot$count)
g <- ggplot(data = data_plot, aes(x = index, y = perc))
g <- g + geom_line(color = "darkblue", lwd = 1)
g <- g + geom_hline(yintercept = 0.5, colour = "red4", lwd = 1, lty = 5)
g <- g + geom_hline(yintercept = 0.9, colour = "red4", lwd = 1, lty = 5)
g <- g + geom_hline(yintercept = 0.95, colour = "red4", lwd = 1, lty = 5)
g <- g + labs(x = "number of words", y = "pecentage %",
              title = "cumulative quantiles")
g <- g + theme_bw()
print(g)

