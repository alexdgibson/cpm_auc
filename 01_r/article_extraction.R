
# rentrez package for extracting relevant articles for screening
# created 2024-07-30

# load in the required libraries
library(tidyverse)
library(rentrez)

# check the pubmed search terms
entrez_db_searchable("pubmed")

# updating the search term to include only those articles which are quartile one
# from scimago_journal_ranking_extraction.R take the 'journal_rank' data frame and extract ISSN for quartlie one journals
# there is an online and print issn, combine both to one column
quartile_one <- journal_rank %>% 
  filter(sjr_best_quartile == "Q1") %>% 
  select(issn) %>% 
  separate(issn, c("issn1","issn2"))

# remove NAs from journals that do not have both a print and online issn
quartile_one_issn <- data.frame(issn = c(quartile_one$issn1, quartile_one$issn2)) %>% na.omit()

# load the updated search filter as string
search_filter <- "(Validat$ OR Predict$.ti. OR Rule$) OR (Predict$ AND (Outcome$ OR Risk$ OR Model$)) OR ((History OR Variable$ OR Criteria OR Scor$ OR Characteristic$ OR Finding$ OR Factor$) AND (Predict$ OR Model$ OR Decision$ OR Identif$ OR Prognos$)) OR (Decision$ AND (Model$ OR Clinical$ OR Logistic Models/)) OR (Prognostic AND (History OR Variable$ OR Criteria OR Scor$ OR Characteristic$ OR Finding$ OR Factor$ OR Model$)) OR “Stratification” OR “ROC Curve”[Mesh] OR “Discrimination” OR “Discriminate” OR “c-statistic” OR “c statistic” OR “Area under the curve” OR “AUC” OR “Calibration” OR “Indices” OR “Algorithm” OR “Multivariable”"

# creating a date filter to search the last 6 months
# change these dates upon search date for preceding 6 months
term <- sprintf('(%s) AND ("2024/01/01"[Date - Publication] : "2024/06/30"[Date - Publication])', search_filter)


# searching pubmed with search filter (note the number of results 'hits' returned)
# this 'hits' number is used later and needs to be recorded for results
entrez_search(db = "pubmed", term = term)

# searching pubmed with search filter (with all results returned saved in a web history object)
articles <- entrez_search(db = "pubmed", term = term, use_history = TRUE)

# check the web history has been stored
articles$web_history

# create a list to store the data
article_summaries <- list()

# iterate through all articles in 50 article search chunks
# change (x,y,z) x for which article to start on, y for total articles to search and z for size of search chunks
# change y to the number of articles previously stored as 'hits' when searching pubmed
# it takes ~ 0.05 sec per article, account for this when searching large volumes
for (seq_start in seq(1, 8000, 50)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles$web_history,
                         rettype = "xml", retmax = 50, retstart = seq_start)
  # add current batch of recs to the list
  article_summaries[[length(article_summaries) + 1]] <- recs
  # print the progress
  cat(seq_start + 49, "article summaries downloaded\r")
}

# save the entire list to a file
# this is the total number of articles on medline in the last 6 months with the updated search term 
save(article_summaries, file = "article.Rdata")

# unlist to get the PMID, essn and issn and filter for quartile one journals 
unlisted_summaries <- unlist(article_summaries, use.names = TRUE)

# turn from a complete unlist to data frame with variable to select PMID, essn and issn
df_summaries <- data.frame(x = unlisted_summaries,
                               y = names(unlisted_summaries))

# select all rows that contain '.uid', '.essn' or '.issn' in variable y
pmid_summaries <- df_summaries[grepl(paste(c(".uid", ".essn", ".issn"), collapse = "|"), df_summaries$y), ]
# remove row names
rownames(pmid_summaries) <- NULL

# remove anything before .uid, .essn or .issn to pivot wider
pmid_summaries$y <- sub(".*\\.(uid|essn|issn)$", "\\1", pmid_summaries$y)

# pivot the data frame wide to have three columns, uid, essn and issn
# note uid is the pubmed identifier
pmid_wide <- pmid_summaries %>% 
  mutate(row_id = rep(1:(n()/3), each = 3)) %>% 
  pivot_wider(names_from = y,
              values_from = x) %>% 
  select(!row_id)

# remove the '-' that is present in the pmid_wide variables for essn and issn
# so that strings will match to cross check
pmid_wide$essn <- gsub("-", "", pmid_wide$essn)
pmid_wide$issn <- gsub("-", "", pmid_wide$issn)

# extract only the journals in article_issn which are quartile one from 'quartile_one_issn'
# this is all the articles which are in quartile one journals from the updated search string
# the difference between this value and the previous all value should be recorded as inclusion/exclusion criteria
quartile_one_pmid <- pmid_wide %>%
  filter(essn %in% quartile_one_issn$issn | issn %in% quartile_one_issn$issn)

# save quartile_one_pmid pmid values only as a csv for upload to abstrackr
save(quartile_one_pmid, file = "")



