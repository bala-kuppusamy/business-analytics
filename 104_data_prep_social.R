
library(tidyverse)
library(ggplot2)
library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)

analyze_text <- function(social) {
  SENTIMENT <- social[1, ]$SENTIMENT
  WEEK <- social[1, ]$WEEK
  print(paste('Doing text analysis for sentiment -', SENTIMENT, ', for week -', WEEK))

  social_corpus <- quanteda::corpus(social$POST)

  addn_stopwords <- c('can', 'just', 'one', 'look', 'get', 'qvc', 'go')
  social_dfm <- quanteda::dfm(social_corpus, remove = c(stopwords::stopwords('english'), addn_stopwords), stem = TRUE, remove_punc = TRUE)
  dim(social_dfm)

  quanteda::topfeatures(social_dfm, 30)

  social_dfm_trim <- quanteda::dfm_trim(social_dfm, min_termfreq = 10, min_docfreq = 2)
  dim(social_dfm_trim)

  social_dfm_matrix <- as.matrix(social_dfm_trim)
  social_dfm_matrix <- social_dfm_matrix[which(quanteda::rowSums(social_dfm_matrix) > 0),]
  dim(social_dfm_matrix)
  social_dfm_trim_2 <- quanteda::as.dfm(social_dfm_matrix)


  social_lda <- topicmodels::LDA(social_dfm_trim_2, k = 4, control = list(seed = 101))
  social_lda_td <- tidytext::tidy(social_lda)

  top_terms <- social_lda_td %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(8, beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, -beta) %>%
    dplyr::mutate(term = reorder(term, beta))

  plot_title <- paste('Sentiment -', SENTIMENT, 'Week -', WEEK)

  p <- top_terms %>%
    ggplot2::ggplot(mapping = aes(term, beta, fill = factor(topic))) +
    ggplot2::geom_bar(stat = 'identity', show.legend = FALSE) +
    ggplot2::facet_wrap(~topic, scales = 'free') +
    ggplot2::labs(title = plot_title) +
    ggplot2::coord_flip()

  p
  ggplot2::ggsave(filename = 'social-text-analysis.png', plot = p)
  print('Saved the text-analysis plot in file -> social-text-analysis.png')
}

execute_text_analysis <- function(social) {
  social <- social[order(social$SENTIMENT), ]

  # social %>%
  #   dplyr::filter(SENTIMENT == 'Strongly negative') %>%
  #   split(c(.$SENTIMENT, .$WEEK)) %>%
  #   purrr::map(.f = analyze_text)

  # executing only for 'Strongly negative' cases, since the amount of data is huge for other categories
  social_sn <- social %>%
    dplyr::filter(SENTIMENT == 'Strongly negative')
  analyze_text(social_sn)
}

prep_social <- function(social, do_text_analysis) {
  social$INTERACTION_DATE <- lubridate::mdy_hms(social$INTERACTION_DATE, tz = 'EST')
  social$SOURCE_TYPE <- factor(social$SOURCE_TYPE)
  social$SENTIMENT <- factor(social$SENTIMENT)
  social$WEEK <- lubridate::week(social$INTERACTION_DATE)

  if (do_text_analysis == TRUE) {
    execute_text_analysis(social)
  }
  return(social)
}
