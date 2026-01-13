# gathering data for piloting of the study
# created Feburary 2025

# load in the required libraries
library(tidyverse)
library(rentrez)
library(XML)

# check the pubmed search terms
entrez_db_searchable("pubmed")

# load in data from journal rankings
load(file = '02_data/journal_rank.rdata')

# updating the search term to include only those articles which are quartile one
# from scimago_journal_ranking_extraction.R take the 'journal_rank' data frame and extract ISSN for quartile one journals
# there is an online and print issn, combine both to one column
quartile_one <- journal_rank %>% 
  filter(sjr_best_quartile == "Q1") %>% 
  select(issn) %>% 
  separate(issn, c("issn1","issn2"))

# remove NAs from journals that do not have both a print and online issn
quartile_one_issn <- data.frame(issn = c(quartile_one$issn1, quartile_one$issn2)) %>% na.omit()

# load the updated search filter as string
search_filter <- "((((((((((((((((c-stat[Title/Abstract]) OR (c-statistic[Title/Abstract])) OR (c-index[Title/Abstract])) OR (concordance stat[Title/Abstract])) OR (concordance statistic[Title/Abstract])) OR (auc[Title/Abstract])) OR (auc value[Title/Abstract])) OR (area under the curve[Title/Abstract])) OR (area under the curve value[Title/Abstract])) OR (area under the receiver operating characteristic curve[Title/Abstract])) OR (receiver operating characteristic curve[Title/Abstract])) OR (area under the receiver operating characteristic curve value[Title/Abstract])) OR (receiver operating characteristic curve value[Title/Abstract])) OR (roc-auc[Title/Abstract])) OR (roc curve[Title/Abstract])) OR (roc curve value[Title/Abstract])) NOT (pharmacokinetic[Title/Abstract])"

# create a few search filters over different time periods to stay under the 10,000 return limit
# creating a date filter to search the last 6 months
# change these dates upon search date for preceding 6 months
term_1 <- sprintf('(%s) AND ("2024/12/01"[Date - Publication] : "2024/12/07"[Date - Publication])', search_filter)

# searching pubmed with search filter (note the number of results 'hits' returned)
# this 'hits' number is used later and needs to be recorded for results
entrez_search(db = "pubmed", term = term_1)

# searching pubmed with search filter (with all results returned saved in a web history object)
articles_1 <- entrez_search(db = "pubmed", term = term_1, use_history = TRUE)

# iterate through all articles in y article search chunks
# change (x,y,z) x for which article to start on, y for total articles to search and z for size of search chunks
# change y to the number of articles previously stored as 'hits' when searching pubmed
# it takes ~ 0.05 sec per article, account for this when searching large volumes

# set api key first
set_entrez_key("e50db130d1a7ba3505f0b957a1b859ff6e08")
api_key = "e50db130d1a7ba3505f0b957a1b859ff6e08"

# create a list to store the data
article_summaries <- list()

# gather all the articles for the first search list
for (seq_start in seq(1, 5000, 500)) {
  recs <- entrez_summary(db = "pubmed", web_history = articles_1$web_history,
                         rettype = "fasta", retmax = 500, retstart = seq_start, version = "2.0")
  article_summaries[[length(article_summaries) + 1]] <- recs
  cat(seq_start + 499, "article summaries downloaded\r")
  Sys.sleep(0.1) # pause for 0.1 second between requests as per rentrez rate-limiting documentation
}

# save the entire list to a file
# this is the total number of articles on medline in the last 12 months with the search term 
save(article_summaries, file = "02_data/pilot_article.Rdata")

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
screening_data <- uids_df %>%
  filter(issn %in% quartile_one_issn$issn)


# save screening data
save(screening_data, file = "02_data/pilot_screening_data.Rdata")


# take the list of articles and return the title and abstract for screening
pubmed <- list()

# loop through each article and take the id, title, abstract, pubdate, journal
for (i in 1:length(as.numeric(screening_data$uid))){
  
  # Fetch and parse the PubMed entry
  detail <- entrez_fetch(db = "pubmed", id = screening_data$uid[i], rettype = "xml", parsed = FALSE, api_key = api_key)
  
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

# create a final dataframe of the articles
pubmed_df <- bind_rows(pubmed)

# create a random list for the articles
# set the seed as 3625
set.seed(3625)

# create the random order of articles
rand_order <- sample_n(pubmed_df, nrow(pubmed_df))

# add a key for Rayyan to process the csv
pubmed_final <- rand_order %>% 
  mutate(key = as.numeric(1:nrow(rand_order))) %>% 
  select(key, pmid, title, abstract, publicationDate, journal)

# save the output for screening
write.csv(pubmed_final, file ="02_data/pilot_pubmed_final.csv", row.names = FALSE)
