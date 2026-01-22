# predict_research_time.R
# plotting prediction research over time to highlight the extent of this research area

# load in libraries
library(tidyverse)
library(ggplot2)
library(rentrez)
library(glue)

# set the time frame for the search
year <- 1950:2025

# combine different search terms and dates to be extracted from rentrez
predict <- glue('"predict*" AND {year}[PDAT]"')
all_search <- glue("{year}[PDAT]")

# get data of published articles from the terms from pubmed 
cpm_count <- tibble(year = year,
                    all_search = all_search,
                    predict = predict) %>% 
  mutate(all_count = map_dbl(all_search, ~entrez_search(db = "pubmed", term = .x)$count),
         predict_count = map_dbl(predict, ~entrez_search(db = "pubmed", term = .x)$count),
         prog_count = map_dbl(prog, ~entrez_search(db = "pubmed", term = .x)$count),
         diag_count = map_dbl(diag, ~entrez_search(db = "pubmed", term = .x)$count))

# plot the figure of predict*
cpm_count %>% 
  select(year, all_count, predict_count, prog_count, diag_count) %>% 
  mutate(pred_prop = (predict_count/all_count)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = pred_prop), linewidth = 1) +
  geom_text(data = . %>% filter(year == max(year)), 
            size = 4,
            aes(y = pred_prop, label = '"predict*"'), hjust = 1, vjust = -1) +
  theme_classic() +
  labs(y = "Proportion",
       x = "Year") +
  geom_hline(yintercept = 0.1, linetype = "dashed", alpha = 0.3)+
  scale_x_continuous(limits = c(1950, 2025), breaks = c(1950, 1975, 2000, 2025))+
  scale_y_continuous(limits = c(0,0.15))+
  theme(text = element_text(size = 12))


# save the figure to figures folder
ggsave(filename = "03_figures/predict_proportion_figure.png", width = 6, height = 4, dpi = 300)



# find the proportion of research in 2025 that uses "predict*"
cpm_count %>% 
  select(year, all_count, predict_count, prog_count, diag_count) %>% 
  mutate(pred_prop = (predict_count/all_count)) %>% 
  filter(year == 2025)

