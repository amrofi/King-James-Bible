This analysis uses the tidy approach elaborated in 'Text Mining with R' by Julia Silge and David Robinson to explore the King James Bible, employing techniques such as term frequency analysis, sentiment analysis and topic modelling to compare the Old and New Testaments and the books therein.

```{r, echo=FALSE, include = FALSE}
library(bibler)
library(tidytext)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(SnowballC)

data("king_james_df")
data(stop_words)
```

## Books of the King James Bible by word count

```{r, echo=FALSE}
book_by_words <- king_james_df %>% 
  unnest_tokens(word, Text) %>% 
  count(Book = King.James.Bible, Testament, word, sort = TRUE) %>% 
  ungroup() %>%
  group_by(Book, Testament) %>% 
  summarize(Words = sum(n)) %>% 
  arrange(desc(Words))

book_by_words$Book <- factor(book_by_words$Book,
                             levels = book_by_words$Book[order(book_by_words$Words)]) # Reorder books according to length
book_by_words$Testament <- factor(book_by_words$Testament, levels = c("Old Testament", "New Testament"))

ggplot(book_by_words, aes(Book, Words, fill = Testament)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(`Old Testament` = "#00BFC4", `New Testament` = "#F8766D")) +
  scale_y_continuous(expand = c(0, 0), label = comma)
```

## Word Frequencies: Old Testament vs New Testament

```{r, echo=FALSE, include = FALSE}
tidy_bible <- king_james_df %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words) %>% 
  anti_join(tibble(word = middle_english_stopwords)) %>% 
  mutate(word = wordStem(word))

total_frequency <- tidy_bible %>% 
  group_by(Testament) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_bible %>% 
              group_by(Testament) %>% 
              summarise(total = n())) %>% 
  mutate(freq = n/total) %>% 
  select(Testament, word, freq) %>% 
  spread(Testament, freq) %>% 
  arrange(`Old Testament`, `New Testament`)
```

```{r, echo=FALSE, message = FALSE, cache = FALSE, results='hide', warning = FALSE}
ggplot(total_frequency, aes(`Old Testament`, `New Testament`, color = abs(`New Testament` - `Old Testament`))) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "gray40", lty = 2) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")
```
