# 07_screening_assessment.R
# examine the agreement between reviewers for the screening

# load in libraries
library(tidyverse)
library(irrCAC)
library(jsonlite)
library(openalexR)

# load in the screening data
df <- read.csv(file = "02_data/03_screening/articles.csv")

# filter data to first 220
# remove extra that second reviewer screened
screened <- df %>% 
  filter(notes != "" & notes != 'RAYYAN-INCLUSION: {"daivd"=>"Included"}' & notes != 'RAYYAN-INCLUSION: {"daivd"=>"Excluded"}')


# get the agreement between reviewers
# turn data frame into review information
screened_clean <- screened %>%
  mutate(rayyan_raw = str_extract(notes,
                                  "RAYYAN-INCLUSION:\\s*\\{.*?\\}"),
         rayyan_raw = str_remove(rayyan_raw, "^RAYYAN-INCLUSION:\\s*"),
         rayyan_raw = str_replace_all(rayyan_raw, "=>", ":")) %>%
  mutate(rayyan = map(rayyan_raw, ~ fromJSON(.x))) %>%
  unnest_wider(rayyan) 

# check if completed for all 
screened_clean %>%
  select(daivd, Alexander, key, title, doi) %>% 
  filter(if_any(everything(), is.na)) %>% view()

# calculate the GWET AC1 statistic between reviewers
agreement <- gwet.ac1.raw(ratings = screened_clean %>% select(daivd, Alexander), weights = "unweighted", conflev = 0.95)

# check the output
agreement

# round to decimal places
round(agreement[[1]][2], digits = 2)
# outcome is GWET AC1 of 0.83 [0.68, 0.84]


# check which articles had disagreement
screened_clean %>% 
  filter(daivd != Alexander) %>% 
  summarise(david_include = sum(daivd == "Included"),
            david_exlude = sum(daivd == "Excluded"),
            alex_included = sum(Alexander == "Included"),
            alex_excluded = sum(Alexander == "Excluded"))

# get the info to save as a csv
screening_diff <- screened_clean %>% 
  filter(daivd != Alexander) %>% 
  select(title, daivd, Alexander)

# save as csv differences in screening
write.csv(screening_diff, file = "02_data/03_screening/01_screening_difference.csv")









# AFTER SCREENING
# load in the screened articles and create hyperlinks to collect PDFs

all_screened <- read.csv("02_data/03_screening/02_screened_articles.csv")

# sperate screener results to only include those which were agreed upon (reviwer two did extra)
all_screened_clean <- all_screened %>%
  mutate(rayyan_raw = str_extract(notes,
                                  "RAYYAN-INCLUSION:\\s*\\{.*?\\}"),
         rayyan_raw = str_remove(rayyan_raw, "^RAYYAN-INCLUSION:\\s*"),
         rayyan_raw = str_replace_all(rayyan_raw, "=>", ":")) %>%
  mutate(rayyan = map(rayyan_raw, ~ fromJSON(.x))) %>%
  unnest_wider(rayyan) 

# filter to include all the appropriately screened articles
all_screened_clean <- all_screened_clean %>% 
  filter(Alexander == "Included") %>% 
  select(title, doi)

write.csv(all_screened_clean, file = "02_data/03_screening/04_included_screened.csv")



# could not get this code to work and may be easier to do by hand


# use OpenAlex to download all of the pdfs that are open access
works <- all_screened_clean$doi

# Fetch in chunks of 50–100
chunks <- split(works, ceiling(seq_along(works)/100))

# use open alex to get all the open access pdf file download links available
works_list <- lapply(chunks, function(chunk) {
  oa_fetch(doi = chunk)
})

# bind them together
works <- bind_rows(works_list) %>% filter(pdf_url != "NA")

# list the links
pdf_urls <- works$pdf_url

# pdf names
pdf_name <- works$doi |>
  gsub("^https?://doi.org/", "", x = _) |>
  gsub("/", "_", x = _)

# Create a vector to store failed URLs
failed_urls <- character(0)

# download all of the pdfs and continue after errors
for(i in seq_along(pdf_urls)) {
  if(!is.na(pdf_urls[i])) {
    
    # Build the full path for the PDF
    file_path <- file.path(out_dir, paste0(pdf_name[i], ".pdf"))
    
    # Attempt to download with error handling
    tryCatch({
      # Download and save the PDF
      GET(
        pdf_urls[i],
        user_agent("Mozilla/5.0"),
        config(followlocation = TRUE),
        write_disk(file_path, overwrite = TRUE)
      )
      
      Sys.sleep(1)  # polite delay
      
    }, error = function(e) {
      # On error, record the failed URL
      failed_urls <<- c(failed_urls, pdf_urls[i])
      message("Failed to download: ", pdf_urls[i], " - ", e$message)
    })
  }
}

# Check failed URLs
if(length(failed_urls) > 0) {
  message("These URLs failed: ", paste(failed_urls, collapse = ", "))
}






# check all of the doi names in the file 02_data/03_screening/01_pdfs
# this step is after the OpenAlex automatic downloads

# make a list of the dois in the file
pdf_saved <- list.files(path = "02_data/03_screening/01_pdfs")

# compare to the list of dois in all_screened_clean
pdf_doi <- pdf_saved |> 
  gsub(".pdf", "", x = _) |>
  gsub("_", "/", x = _)

# remove the ones already saved as pdfs
missing_dois_pdf <- all_screened_clean %>%
  filter(!doi %in% pdf_doi)

# save the remaining as a csv to go get manually
write.csv(missing_dois_pdf, file = "02_data/03_screening/05_manual_pdf.csv")




# check all of the doi names in the file 02_data/03_screening/01_pdfs
# check what articles are left to be collected remaining after automatic and manual screening

# make a list of the dois in the file
pdf_saved <- list.files(path = "02_data/03_screening/01_pdfs")

# load in all articles
all_screened_clean <- read.csv(file = "02_data/03_screening/04_included_screened.csv")

# compare to the list of dois in all_screened_clean
pdf_doi <- pdf_saved |> 
  gsub(".pdf", "", x = _) |>
  gsub("_", "/", x = _)

# remove the ones already saved as pdfs
missing_dois_pdf <- all_screened_clean %>%
  filter(!doi %in% pdf_doi)

# save the remaining as a csv to go get manually
write.csv(missing_dois_pdf, file = "02_data/03_screening/06_reamining_pdf.csv")






# check final list of pdfs that are available
# make a list of the dois in the file
pdf_saved <- list.files(path = "02_data/03_screening/01_pdfs")

# load in all articles
all_screened_clean <- read.csv(file = "02_data/03_screening/04_included_screened.csv")

# compare to the list of dois in all_screened_clean
pdf_doi <- pdf_saved |> 
  gsub(".pdf", "", x = _) |>
  gsub("_", "/", x = _)

# remove the ones already saved as pdfs
missing_dois_pdf <- all_screened_clean %>%
  filter(!tolower(doi) %in% tolower(pdf_doi))

# list of articles that were not accessible
not_accessible <- c("10.1016/j.ymthe.2025.06.041", "10.3171/2024.12.JNS242496", "10.1159/000549282", "10.14309/ajg.0000000000003319", "10.1016/j.cgh.2025.05.029",
                    "10.1055/a-2530-7553", "10.3171/2025.3.JNS242330", "10.1016/j.hrthm.2025.02.021", "10.1016/j.acra.2025.05.050", "10.1016/j.bja.2025.05.048", "10.1016/j.jvs.2025.02.030", "10.1097/SLA.0000000000006815", "10.5435/JAAOS-D-24-00494", "10.1515/cclm-2025-0705", "10.1016/j.gassur.2025.102262",
                    "10.1016/j.healun.2025.07.004", "10.1016/j.acra.2025.10.015", "10.1016/j.gassur.2025.102245", "10.1212/WNL.0000000000210308", '10.1159/000549578', 
                    "10.1212/WNL.0000000000214349", "10.1016/j.acra.2025.07.047", "10.1016/j.joms.2025.04.010", "10.1016/j.hrthm.2025.01.009", "10.1016/j.acra.2025.07.039",
                    "10.1016/j.gassur.2025.101980", "10.1177/13872877251359889", "10.1016/j.acra.2025.04.057", "10.1097/HEP.0000000000001462", "10.1016/j.labinv.2025.104101", 
                    "10.1097/EJA.0000000000002251", "10.1016/j.acra.2025.05.031", "10.1016/j.acra.2025.07.043", "10.1016/j.modpat.2025.100852", "10.1055/a-2707-2862", 
                    "10.1097/INF.0000000000004964", "10.1016/j.acra.2025.06.010", "10.1097/SHK.0000000000002656", "10.1016/j.acra.2025.06.025", "10.1016/j.arthro.2025.01.034", 
                    "10.1161/HYPERTENSIONAHA.125.25022", "10.1016/j.tjnut.2025.01.019", "10.1016/j.joen.2025.05.020", "10.1097/JU.0000000000004858", "10.1016/j.acra.2025.03.054", 
                    "10.1177/13872877251333086", "10.1016/j.soard.2025.07.014", "10.3171/2025.2.JNS242210", "10.1016/j.acra.2025.05.059", "10.1016/j.acra.2025.08.054", 
                    "10.1016/j.acra.2024.12.040", "10.1097/PCC.0000000000003772", "10.1055/a-2525-4622", "10.3171/2024.10.JNS241749", "10.1016/j.athoracsur.2025.10.028", 
                    "10.1016/j.acra.2025.03.039", "10.1097/JU.0000000000004437", "10.1016/j.acra.2025.04.017", "10.1016/j.acra.2025.02.047", "10.1097/AOG.0000000000006014", 
                    "10.1016/j.jtcvs.2025.08.030", "10.1016/j.acra.2025.08.066", "10.1016/j.acra.2025.03.052", "10.1097/ALN.0000000000005480", "10.1016/j.acra.2024.12.074", 
                    "10.1016/j.modpat.2025.100877", "10.1016/j.acra.2024.12.067", '10.1016/j.acra.2025.04.071', "10.1016/j.jvs.2025.05.041", "10.1097/GME.0000000000002493",
                    "10.1016/j.cmi.2025.06.021", "10.3171/2024.11.PEDS24272", "10.1016/j.acra.2025.08.002", "10.1016/j.acra.2025.03.046", "10.1097/QAI.0000000000003798", 
                    "10.1097/HEP.0000000000001433", "10.1227/neu.0000000000003668", "10.1016/j.jvs.2025.06.004", "10.1136/rapm-2024-106003", "10.1183/13993003.00519-2025", 
                    "10.1016/j.hpb.2025.12.007", "10.1097/SLA.0000000000006838", "10.1097/RLI.0000000000001193", "10.1016/j.cgh.2025.06.025", "10.1109/TPAMI.2025.3545639", 
                    "10.1159/000545943", "10.1136/lupus-2025-001795", "10.1016/j.acra.2025.05.007", "10.1016/j.soard.2025.06.019", "10.1016/j.hpb.2025.09.005", "10.3171/2025.1.PEDS2436", 
                    "10.1037/pst0000570", "10.1016/j.bja.2025.11.010", "10.1016/j.acra.2025.04.025", "10.1016/j.acra.2025.07.013")

# list of articles that remain to check
last <- missing_dois_pdf %>% 
  filter(!tolower(doi) %in% tolower(not_accessible))

# all articles are accounted for as downloaded or not accessible
#1399 downloaded and 95 not accessible which is 1494 which is all_screened_clean




# the random list to extract data in for final articles

extract_list <- all_screened_clean %>% 
  filter(!doi %in% not_accessible) %>% 
  mutate(pdf = gsub("/", "_", doi))

write.csv(extract_list, file = "02_data/extract_data_list.csv")
  
