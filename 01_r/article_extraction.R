
# rentrez package for extracting relevant articles for screening
# created 2024-07-30

# load in the required libraries
library(rentrez)

# check the pubmed search terms
entrez_db_searchable("pubmed")

# load the ingui search filter as string
ingui <- "(Validat$ OR Predict$.ti. OR Rule$) OR (Predict$ AND (Outcome$ OR Risk$ OR Model$)) OR ((History OR Variable$ OR Criteria OR Scor$ OR Characteristic$ OR Finding$ OR Factor$) AND (Predict$ OR Model$ OR Decision$ OR Identif$ OR Prognos$)) OR (Decision$ AND (Model$ OR Clinical$ OR Logistic Models/)) OR (Prognostic AND (History OR Variable$ OR Criteria OR Scor$ OR Characteristic$ OR Finding$ OR Factor$ OR Model$))"

# creating a date filter to search the last 30 days
# change these dates upon search date for preceding 30 days
term <- sprintf('(%s) AND ("2024/05/01"[Date - Publication] : "2024/06/01"[Date - Publication])', ingui)

# searching pubmed with search filter (note the number of 'results 'hits' returned)
entrez_search(db = "pubmed", term = term)

# searching pubmed with search filter (with all results returned)
ids <- entrez_search(db = "pubmed", term = term, retmax = 51812)