library(ggplot2)

barplot_terms <- function(df, title = "") {
    g <- ggplot(data = head(arrange(df, desc(count)), 10),
                aes(x = reorder(terms, desc(count)), y = count))
    g <- g + geom_bar(stat = "identity", fill = "darkblue")
    g <- g + labs(x = "frequency", y = "words",
                  title = title)
    g <- g + theme_bw()
    g <- g + theme(axis.text.x = element_text(angle = 90, size = 10))
    return(g)
}
