# 8_data_code_availability.R

# load in libraries
library(dplyr)
library(stringr)
library(oddpub) # for data and code
library(roadoi)



# Converts PDFs contained in one folder to txt-files: (recursive off; do not do sub-folders)
oddpub::pdf_convert(pdf_folder = "04_pdfs", recursive = FALSE, overwrite_existing_files = FALSE, output_folder = "04_pdfs/01_output") # takes a while

# Loads all text files from given folder
pdf_text_sentences <- pdf_load("04_pdfs/01_output")

# check for open data and code
open_data_results <- open_data_search(pdf_text_sentences) # takes a while



# add DOI do use later
open_data_results = mutate(open_data_results, 
                           doi = str_remove(article, '\\.txt$'))


# save the output
# save(open_data_results, file = '02_data/02_pilot/data_code_availability.Rdata')



# check for open access using the roadoi package
# load in the data
load(file = '02_data/02_pilot/data_code_availability.Rdata')

# check open access
open_access <- oadoi_fetch(dois = str_replace_all(open_data_results$doi, "_", "/"),
                           email = "") # put in email to use

# save the output
# save(open_access, file = "02_data/02_pilot/pilot_open_access_data.Rdata")

# load in data
load(file = '02_data/02_pilot/pilot_open_access_data.Rdata')

# get summary data
open_access %>% 
  summarise(TRUE_OA = sum(is_oa == "TRUE"),
            FALSE_OA = sum(is_oa == "FALSE"),
            perc_OA = TRUE_OA/(TRUE_OA+FALSE_OA))
