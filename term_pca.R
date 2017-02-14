library(tidyverse)
library(tidytext)
library(irlba)
# devtools::install_github("kbenoit/quanteda") 
library(quanteda)

data(stop_words)

papers <- read_csv("full_biorxiv_data.csv") %>% 
  select(titles, abstracts)

word_counts <- papers %>% 
  unnest_tokens(word, abstracts) %>% 
  count(titles, word, sort = TRUE) %>% 
  ungroup()

word_freqs <- word_counts %>% 
  anti_join(stop_words) %>% 
  bind_tf_idf(word, titles, n) 

term_mat <- word_freqs %>% 
  cast_dfm(titles, word, tf) %>% 
  as.matrix()

# term_pca <- prcomp(term_mat,center = TRUE, scale. = TRUE) 

term_pca <- term_mat %*% irlba(term_mat, nv=5, nu=0, center=colMeans(term_mat), right_only=TRUE)$v

term_pca_df <- as_data_frame(term_pca) %>% 
  rename_(.dots = setNames(names(.), paste0("PC", 1:5))) %>% 
  mutate(title = rownames(term_pca))

ggplot(term_pca_df, aes(x = PC1, y = PC2)) + 
  geom_point(alpha = 0.2)

library(plotly)

packageVersion('plotly')

plot_ly(term_pca_df, x = ~PC1, y = ~PC2, z = ~PC3, opacity = 0.2,
        text = ~paste('Title:', title)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))
