
# code to extract scimago journal rankings to filter articles for data article extraction
# created 2024-07-31

# load in the required libraries
library(tidyverse)
library(janitor)
library(readxl)

# function to get data from scimago website

journal_url <- function(year) {
  paste0(
    "https://www.scimagojr.com/journalrank.php?year=",
    year,
    "&out=xls"
  )
}

# set year of rankings to retrieve
# last available is 2023

year <- 2023

# gather the data for journal rankings

journal_rank <- list()

for (i in seq_along(year)) {
  # load the year into data
  dfi <- suppressMessages(suppressWarnings(
    read_csv2(url(journal_url(year[i])))
  )) %>% clean_names()
  
  # fix the uniquiely named column of total docs
  colnames(dfi)[9] <-
    colnames(dfi)[9] %>%
    str_replace("[0-9]+", "year")
  
  # write the temp dfi into the list
  journal_rank[[i]] <- dfi
  
  # name the dfi in the list
  
  names(journal_rank)[i] <- year[i]
}

journal_rank <- journal_rank %>% bind_rows(.id = "year")

# save to file
save(journal_rank, file = "")
