
# rentrez package for extracting relevant articles for screening
# created 2024-07-30

# load in the required libraries
library(tidyverse)
library(rentrez)
library(XML)


# check the pubmed search terms
entrez_db_searchable("pubmed")

# load in data from journal rankings
load(file = '02_data/q_one_journals.rdata')

# updating the search term to include only those articles which are quartile one
# from scimago_journal_ranking_extraction.R take the 'journal_rank' data frame and extract ISSN for quartile one journals
# there is an online and print issn, combine both to one column
quartile_one <- q_one_journals %>% 
  filter(sjr_best_quartile == "Q1") %>% 
  select(issn) %>% 
  separate(issn, c("issn1","issn2"))

# remove NAs from journals that do not have both a print and online issn
quartile_one_issn <- data.frame(issn = c(quartile_one$issn1, quartile_one$issn2)) %>% na.omit()

# load the updated search filter as string
search_filter <- "((((((((((((((((c-stat[Title/Abstract]) OR (c-statistic[Title/Abstract])) OR (c-index[Title/Abstract])) OR (concordance stat[Title/Abstract])) OR (concordance statistic[Title/Abstract])) OR (auc[Title/Abstract])) OR (auc value[Title/Abstract])) OR (area under the curve[Title/Abstract])) OR (area under the curve value[Title/Abstract])) OR (area under the receiver operating characteristic curve[Title/Abstract])) OR (receiver operating characteristic curve[Title/Abstract])) OR (area under the receiver operating characteristic curve value[Title/Abstract])) OR (receiver operating characteristic curve value[Title/Abstract])) OR (roc-auc[Title/Abstract])) OR (roc curve[Title/Abstract])) OR (roc curve value[Title/Abstract])) NOT (pharmacokinetic[Title/Abstract])"

# create a few search filters over different time periods to stay under the 10,000 return limit
# creating a date filter to search the last 
# change these dates upon search date for preceding 6 months
term_jan1 <- sprintf('(%s) AND ("2025/01/01"[Date - Publication] : "2025/01/01"[Date - Publication])', search_filter) # Jan 1
term_jan2 <- sprintf('(%s) AND ("2025/01/02"[Date - Publication] : "2025/01/31"[Date - Publication])', search_filter) # Jan 2
term_feb <- sprintf('(%s) AND ("2025/02/01"[Date - Publication] : "2025/02/28"[Date - Publication])', search_filter) # Feb
term_mar <- sprintf('(%s) AND ("2025/03/01"[Date - Publication] : "2025/03/31"[Date - Publication])', search_filter) # Mar
term_apr <- sprintf('(%s) AND ("2025/04/01"[Date - Publication] : "2025/04/30"[Date - Publication])', search_filter) # Apr
term_may <- sprintf('(%s) AND ("2025/05/01"[Date - Publication] : "2025/05/31"[Date - Publication])', search_filter) # May
term_jun <- sprintf('(%s) AND ("2025/06/01"[Date - Publication] : "2025/06/30"[Date - Publication])', search_filter) # Jun
term_jul <- sprintf('(%s) AND ("2025/07/01"[Date - Publication] : "2025/07/31"[Date - Publication])', search_filter) # Jul
term_aug <- sprintf('(%s) AND ("2025/08/01"[Date - Publication] : "2025/08/31"[Date - Publication])', search_filter) # Aug
term_sep <- sprintf('(%s) AND ("2025/09/01"[Date - Publication] : "2025/09/30"[Date - Publication])', search_filter) # Sep
term_oct <- sprintf('(%s) AND ("2025/10/01"[Date - Publication] : "2025/10/31"[Date - Publication])', search_filter) # oct
term_nov <- sprintf('(%s) AND ("2025/11/01"[Date - Publication] : "2025/11/30"[Date - Publication])', search_filter) # Nov
term_dec <- sprintf('(%s) AND ("2025/12/01"[Date - Publication] : "2025/12/31"[Date - Publication])', search_filter) # Dec

# searching pubmed with search filter (note the number of results 'hits' returned)
# this 'hits' number is used later and needs to be recorded for results
entrez_search(db = "pubmed", term = term_jan1)
entrez_search(db = "pubmed", term = term_jan2)
entrez_search(db = "pubmed", term = term_feb)
entrez_search(db = "pubmed", term = term_mar)
entrez_search(db = "pubmed", term = term_apr)
entrez_search(db = "pubmed", term = term_may)
entrez_search(db = "pubmed", term = term_jun)
entrez_search(db = "pubmed", term = term_jul)
entrez_search(db = "pubmed", term = term_aug)
entrez_search(db = "pubmed", term = term_sep)
entrez_search(db = "pubmed", term = term_oct)
entrez_search(db = "pubmed", term = term_nov)
entrez_search(db = "pubmed", term = term_dec)

# searching pubmed with search filter (with all results returned saved in a web history object)
articles_1 <- entrez_search(db = "pubmed", term = term_jan1, use_history = TRUE)
articles_2 <- entrez_search(db = "pubmed", term = term_jan2, use_history = TRUE)
articles_3 <- entrez_search(db = "pubmed", term = term_feb, use_history = TRUE)
articles_4 <- entrez_search(db = "pubmed", term = term_mar, use_history = TRUE)
articles_5 <- entrez_search(db = "pubmed", term = term_apr, use_history = TRUE)
articles_6 <- entrez_search(db = "pubmed", term = term_may, use_history = TRUE)
articles_7 <- entrez_search(db = "pubmed", term = term_jun, use_history = TRUE)
articles_8 <- entrez_search(db = "pubmed", term = term_jul, use_history = TRUE)
articles_9 <- entrez_search(db = "pubmed", term = term_aug, use_history = TRUE)
articles_10 <- entrez_search(db = "pubmed", term = term_sep, use_history = TRUE)
articles_11 <- entrez_search(db = "pubmed", term = term_oct, use_history = TRUE)
articles_12 <- entrez_search(db = "pubmed", term = term_nov, use_history = TRUE)
articles_13 <- entrez_search(db = "pubmed", term = term_dec, use_history = TRUE)





# iterate through all articles in y article search chunks
# change (x,y,z) x for which article to start on, y for total articles to search and z for size of search chunks
# change y to the number of articles previously stored as 'hits' when searching pubmed
# it takes ~ 0.05 sec per article, account for this when searching large volumes

# set api key first
set_entrez_key("e50db130d1a7ba3505f0b957a1b859ff6e08")
api_key = "e50db130d1a7ba3505f0b957a1b859ff6e08"

# create a list to store the data
article_summaries <- list()

# gather all the articles for the first search list, the first jan of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_1$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}




# gather all the articles for the first search list, the second jan of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_2$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}




# gather all the articles for the first search list, Feb of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_3$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Mar of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_4$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Apr of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_5$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, May of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_6$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Jun of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_7$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Jul of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_8$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Aug of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_9$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Sep of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_10$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Oct of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_11$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Nov of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_12$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}



# gather all the articles for the first search list, Dec of 2025
# in seq(x,y,z) change y to the "hits" returned for that month
for (seq_start in seq(1, 10000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_13$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}







# save the entire list to a file
# this is the total number of articles on pubmed in the last 6 months with the search term 
save(article_summaries, file = "02_data/article.Rdata")



# from article summaries, take the uid, issn, essn, pubdate, epubdate, title and pubtype from the studies
# create a function to go through each esummary and take the relevant item from all article lists
uids <- lapply(article_summaries, function(sublist) {
  lapply(sublist, function(x) {
    list(uid = x[[1]][[1]], # uid
         pubdate = x[[2]][[1]], # pubdate
         epubdate = x[[3]][[1]], #electronic pubdate
         issn = x[[14]][[1]], # issn
         essn = x[[15]][[1]], # essn
         pubtype = x[[17]][[1]], # pubtype
         journal_name = x[[24]][[1]]) # full journal name
  })
})

# combine into a single data frame
uids_df <- as.data.frame(do.call(rbind, lapply(uids, function(lst) do.call(rbind, lst))), row.names = FALSE)

# remove the hyphen in the issn and essn to filter through Q1 journals
uids_df$issn <- gsub("-", "", uids_df$issn)
uids_df$essn <- gsub("-", "", uids_df$essn)


# from the quartile one journals, select them in the data set
screening_journal_data <- uids_df %>%
  filter(issn %in% quartile_one_issn$issn)

# get total number of articles found with the search filter for 2025
nrow(uids_df) # total articles1
nrow(screening_journal_data) # total articles in Q!

# save screening journal data
save(screening_journal_data, file = "02_data/screening_journal_data.Rdata")



# take a random sample of the articles to get
# set seed for reproducibility
(nrow(screening_journal_data)*0.1) # 2581.2 rounded up to 2582
set.seed(3625)
final_screening_journal_data <- slice_sample(screening_journal_data, n = 2602)

# save screening journal data
save(final_screening_journal_data, file = "02_data/final_screening_journal_data.Rdata")


# take the list of articles and return the title and abstract for screening
pubmed <- list()

# loop through each article and take the id, title, abstract, pubdate, journal
for (i in 1:length(as.numeric(final_screening_journal_data$uid))){
  
  # Fetch and parse the PubMed entry
  detail <- entrez_fetch(db = "pubmed", id = final_screening_journal_data$uid[i], rettype = "xml", parsed = FALSE, api_key = api_key)
  
  parsed <- XML::xmlParse(detail)
  
  # Extract relevant data from the parsed XML
  pmid <- xpathSApply(parsed, "//PubmedArticle/PubmedData/ArticleIdList/ArticleId[@IdType='pubmed']", xmlValue)
  title <- xpathSApply(parsed, "//PubmedArticle/MedlineCitation/Article/ArticleTitle", xmlValue)
  abstract_nodes <- xpathSApply(parsed, "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText", xmlValue)
  abstract <- str_c(abstract_nodes, collapse = " ")
  pubdate <- xpathSApply(parsed, "//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Year", xmlValue)
  journal <- xpathSApply(parsed, "//PubmedArticle/MedlineCitation/Article/Journal/Title", xmlValue)
  
  # Create a data frame of the current article's details
  temp <- list(pmid = pmid,
               title = ifelse(length(title) > 0, title, NA),
               abstract = ifelse(length(abstract) > 0, abstract, NA),
               publicationDate = ifelse(length(pubdate) > 0, pubdate, NA),
               journal = ifelse(length(journal) > 0, journal, NA))
  
  # Append the current article's details to the overall data frame
  pubmed <- append(pubmed, list(temp))
  
  cat(i + 1, "title and abstracts downloaded\r")
  Sys.sleep(0.1)  # Pause for 0.1 second between requests to respect rate-limiting
}

# create a final data frame of the articles
pubmed_df <- bind_rows(pubmed)

# add a key for Rayyan to process the csv
pubmed_final <- pubmed_df %>% 
  mutate(key = as.numeric(1:nrow(pubmed_df))) %>% 
  select(key, pmid, title, abstract, publicationDate, journal)

# save the output for screening
write.csv(pubmed_final, file ="02_data/pubmed_final.csv", row.names = FALSE)







