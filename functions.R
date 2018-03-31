# Load special R libraries
library(tidyverse)      
library(tidytext)       
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(tokenizers)
library(data.table)
require(stringi)

#Cloud Funtion
quick_wordcloud <- function(text, remove_words_vector, find_only){
  # Apply find_only Filter 
  docs <- grep(find_only, text, value=T)
  # Create Vector Source
  docs <- Corpus(VectorSource(docs))
  # Clean that text
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("en"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, remove_words_vector) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  #docs <- tm_map(docs, stemDocument)
  # Create a TDM
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  # Word Cloud Baby
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}  

# Plot word freq function
quick_wordfreq <- function(text, remove_words_vector, main_text){
  docs <- Corpus(VectorSource(text))
  # Clean that text
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("en"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, remove_words_vector) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  # Create a TDM
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  # Plot
  barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
          col ="lightblue", main = main_text,
          ylab = "Word frequencies")
}

#Plot Sentiment Graphs
sentiment_graph <- function(titles, books){
      series <- tibble()
      for(i in seq_along(titles)) {
        
        clean <- tibble(chapter = seq_along(books[[i]]),
                        text = books[[i]]) %>%
          unnest_tokens(word, text) %>%
          mutate(book = titles[i]) %>%
          select(book, everything())
        
        series <- rbind(series, clean)
      }
      
      #Set factor to keep books in order of publication
      series$book <- factor(series$book, levels = rev(titles))
      
      #Plot all titles 
      series %>%
        group_by(book) %>% 
        mutate(word_count = 1:n(),
               index = word_count %/% 500 + 1) %>% 
        inner_join(get_sentiments("bing")) %>%
        count(book, index = index , sentiment) %>%
        ungroup() %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative,
               book = factor(book, levels = titles)) %>%
        ggplot(aes(index, sentiment, fill = book)) +
        geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
        facet_wrap(~ book, ncol = 2, scales = "free_x")
  }

#Print emotional counts
print_em_counts <- function(titles, books){
  series <- tibble()
  for(i in seq_along(titles)) {
    
    clean <- tibble(chapter = seq_along(books[[i]]),
                    text = books[[i]]) %>%
      unnest_tokens(word, text) %>%
      mutate(book = titles[i]) %>%
      select(book, everything())
    
    series <- rbind(series, clean)
  }
  
  #Set factor to keep books in order of publication
  series$book <- factor(series$book, levels = rev(titles))
  
  
  #Print emotional counts
  series %>%
    right_join(get_sentiments("nrc")) %>%
    filter(!is.na(sentiment)) %>%
    count(sentiment, sort = TRUE)
}


#Bar chart of word freq
bar_chart_em_freq <- function(titles, books){
  
  series <- tibble()
  
  for(i in seq_along(titles)) {
    
    clean <- tibble(chapter = seq_along(books[[i]]),
                    text = books[[i]]) %>%
      unnest_tokens(word, text) %>%
      mutate(book = titles[i]) %>%
      select(book, everything())
    
    series <- rbind(series, clean)
  }
  
  #Set factor to keep books in order of publication
  series$book <- factor(series$book, levels = rev(titles))
  
  bing_word_counts <- series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
  bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(75) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()
}


#Book Sent Plot
book_sent <- function(book){
tibble(text = book) %>% 
  unnest_tokens(sentence, text, token = "sentences")

ps_sentences <- tibble(chapter = 1:length(book),
                       text = book) %>% 
  unnest_tokens(sentence, text, token = "sentences")


book_sent <- ps_sentences %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(desc(sentiment))

ggplot(book_sent, aes(index, factor(chapter, levels = sort(unique(chapter), decreasing = TRUE)), fill = sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Chapter Progression", y = "Chapter") +
  ggtitle("Sherlock Holmes and the Baker Street Irregulars: In Search of Watson",
          subtitle = "Summary of the net sentiment score as you progress through each chapter") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")
}

