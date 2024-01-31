# function used to make bar plot of most frequent positive/negative words
plot_sentiment <- function(data, title){
  data %>% group_by(sentiment) %>%
    top_n(7,n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, ncol = 2) +
    labs(y = title,
         x = NULL) +
    coord_flip()+
    theme(axis.text=element_text(size=8))
}