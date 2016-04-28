library(dplyr)

################################################################################
# function for prediction

#' This function provides a data.frame which contains possible predicted words
#' ordered by probability. The words are predicted by the previous n-1 words.
#'
#' @param input: a character string conaining the input sentence
#' @param n: number of words of the n-gram with the predicted word
#' @param df_max: a data.frame containing a n-gram containing the fields
#' "terms", "count" amd "prob" (as calculated above)
#' @param df_min: a data.frame containing a (n-1)-gram containing the fields
#' "terms", "count" amd "prob" (as calculated above)
#' @return df: a data.frame which contains:
#' "words": possible predicted words
#' "count": number of ngrams with the predicted word in the sample
#' "probability": probability of the predicted word
#' @author Florian Ulmar

pred_ngram <- function(input, df_min, df_max, n) {
    
    # remove everything except literals and spaces
    input <- expand_contractions(input)
    input <- gsub("[^a-zA-Z ]", "", input)
    input <- gsub("\\s+", " ", input) # remove multiple spaces
    input <- tolower(input)
    
    # only the last n - 1 words of the input
    term <- tail(unlist(strsplit(input, " ")), n - 1)
    # if number of words is not too small
    if(length(term) < n - 1) {return()}
    # looking for n-grams which beginn with term
    term <- paste("^\\b", paste(term, collapse = " "), "\\b", sep = "")
    
    # find = rows of df_max which contain term
    df_max$terms <- as.character(df_max$terms)
    df_max <- df_max[grep(term, df_max$terms),]
    if(nrow(df_max) == 0) {
        return()
    }
    
    # find = rows of df_min which contain term
    df_min$terms <- as.character(df_min$terms)
    df_min <- df_min[grep(term, df_min$terms),]
    if(nrow(df_min) == 0) {
        return()
    }
    
    # vector of possible predictors of df_max
    mat_max <- matrix(unlist(strsplit(df_max$terms, " ")), ncol = n, byrow = T)
    
    # data frame with possible words
    df <- data.frame(
        "word" = mat_max[, n],
        "count" = df_max$count,
        "prob" = df_max$count / max(df_min$count)
    )
    
    df <- df %>%
        arrange(desc(prob)) %>%
        mutate(word = as.character(word))
    
    return(df)
}

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
