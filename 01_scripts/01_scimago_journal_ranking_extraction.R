
# code to extract scimago journal rankings to filter articles for data article extraction
# created 2024-07-31

# load in the required libraries
library(tidyverse)
library(janitor)
library(readxl)

# function to get data from scimago website

journal_rankings <- read_delim("02_data/scimagojr_2024.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names()


# filter for Q1 ranked journals
q_one_journals <- journal_rankings %>% filter(sjr_best_quartile == "Q1")

# save to file
save(q_one_journals, file = "02_data/q_one_journals.Rdata")
