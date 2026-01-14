print_word_cloud = function(text){
  #' Function for printing a basic word cloud
  #'
  #' @param text A class of character or character vector
  #' @return a word cloud in the viewer pane
  #' @export
  #'
  #' @examples
  #' print_word_cloud(text = c("a","b",letters[1:13]))

  wc_df <- text %>%
    as.character() %>%
    tolower() %>%
    trimws() %>%
    table() %>%
    as.data.frame() %>%
    setNames(c("word", "freq")) %>%
    arrange(desc(freq))

  wordcloud(
    words = wc_df$word,
    freq  = wc_df$freq,
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
}
