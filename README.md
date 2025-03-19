# cpm_auc
 Examination of AUC values in clinical prediction models



# R scripts and their purpose

scimago_journal_ranking_extraction.R
extract the journal rankings from the scimago website so that these journals can be used in the article_extraction.R file

article_extraction.R
From the search term, using rentrez package extract all the relevant articles for screening

simulate_roc_cruves.R
simulate 50 different ROC cruves to validate the data extraction of AUC values from the plotdigitizer software

extract_auc_roc.R
from the simulate_roc_curves.R file, extracting the auc value and comparing against the known value. Also creates histogram plot of the data.

pilot_articles.R
only for finding the articles that are required for the pilot of articles

protocol_flowcharts.R
all of the flowcharts that are made for this project

all_figures.R
creating all the associated histograms and bar charts that are required for the article
also has the expected distributions that would be required for calculating the residuals



