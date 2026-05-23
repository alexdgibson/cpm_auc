# 09_claude_validate.R
# claude extraction validation

# load libraries
library(tidyverse)
library(irrCAC)
library(dplyr)


# # create a data frame to save any files that fail to import
# failed_files <- c()
# 
# # make all imports character for any inconsistencies
# safe_read <- function(file) {
#   tryCatch(
#     read_csv(file) |> mutate(across(everything(), as.character)),
#     error = function(e) {
#       failed_files <<- c(failed_files, file)
#       NULL
#     }
#   )
# }
# 
# # load in using the safe load
# claude_extract <- list.files("02_data/03_screening/01_pdfs/02_claude_extract/03_prompt_3/02_csv/",
#                  pattern = "^article_extraction",
#                  full.names = TRUE) |>
#   lapply(safe_read) |>
#   Filter(Negate(is.null), x = _) |>
#   bind_rows()
# 
# # #save the combined files into one file
# save(claude_extract, file = "02_data/claude_extracted.Rdata")

# load in the claude extracted data
load(file = "02_data/claude_extracted.Rdata")

# load in the list of DOIs that were extracted
doi_list <- read.csv(file = "02_data/extract_data_list.csv")

# read in the the hand extracted data
hand_extract <- read.csv(file = "02_data/01_data_extraction.csv")




# change the dois so they are all the same in a _ format
claude_extract$doi <- tolower(gsub("[./()]", "_", sub("\\.pdf$", "", claude_extract$doi)))
doi_list$doi      <- tolower(gsub("[./()]", "_", doi_list$doi))
hand_extract$doi  <- tolower(gsub("[./()]", "_", hand_extract$doi))



# check the llm extracted for 1,399 unique DOIs
length(unique(claude_extract$doi))

# check the hand extraction DOIs match those in the extraction list
table(unique(hand_extract$doi) %in% doi_list$pdf)

# check the number of DOIs in the hand extract are found in the claude extract
table(unique(gsub("\\.", "_", hand_extract$doi)) %in% sub("\\.pdf$", "", claude_extract$doi))


# Find DOIs that don't match
extracted_dois <- unique(sub("\\.pdf$", "", claude_extract$doi))
reference_dois <- gsub("[./]", "_", doi_list$doi)

# Show the mismatches
extracted_dois[!extracted_dois %in% reference_dois]





# check the fomrat of the dataframes

# change the values so they are the same type
# claude
claude_extract$auc <- as.numeric(claude_extract$auc)
claude_extract$auc_ci_low <- as.numeric(claude_extract$auc_ci_low)
claude_extract$auc_ci_upper <- as.numeric(claude_extract$auc_ci_upper)
claude_extract$sample_size <- as.numeric(claude_extract$sample_size)
claude_extract$sens <- as.numeric(claude_extract$sens)
claude_extract$sens_ci_lower <- as.numeric(claude_extract$sens_ci_lower)
claude_extract$sens_ci_upper <- as.numeric(claude_extract$sens_ci_upper)
claude_extract$spec <- as.numeric(claude_extract$spec)
claude_extract$spec_ci_lower <- as.numeric(claude_extract$spec_ci_lower)
claude_extract$spec_ci_upper <- as.numeric(claude_extract$spec_ci_upper)
claude_extract$article <- as.numeric(claude_extract$article)

# check that the data are in the same decimal (i.e., 0.778 not 77.8)
claude_extract <- claude_extract %>%
  mutate(sens = ifelse(sens <= 1, sens * 100, sens))

claude_extract <- claude_extract %>%
  mutate(sens_ci_lower = ifelse(sens_ci_lower <= 1, sens_ci_lower * 100, sens_ci_lower))

claude_extract <- claude_extract %>%
  mutate(sens_ci_upper = ifelse(sens_ci_upper <= 1, sens_ci_upper * 100, sens_ci_upper))

claude_extract <- claude_extract %>%
  mutate(spec = ifelse(spec <= 1, spec * 100, spec))

claude_extract <- claude_extract %>%
  mutate(spec_ci_lower = ifelse(spec_ci_lower <= 1, spec_ci_lower * 100, spec_ci_lower))

claude_extract <- claude_extract %>%
  mutate(spec_ci_upper = ifelse(spec_ci_upper <= 1, spec_ci_upper * 100, spec_ci_upper))

str(claude_extract)

# for hand extraction
hand_extract$auc <- as.numeric(hand_extract$auc)
hand_extract$sample_size <- as.numeric(hand_extract$sample_size)
hand_extract$sens <- as.numeric(hand_extract$sens)
hand_extract$spec <- as.numeric(hand_extract$spec)
hand_extract$article <- as.numeric(hand_extract$article)


# check how many articles between hand and claude were the same auc
hand_include <- hand_extract %>% filter(model != "na")

count_comparison <- full_join(
  claude_extract %>% count(doi, name = "n_claude"),
  hand_include   %>% count(doi, name = "n_hand"),
  by = "doi"
) %>%
  mutate(
    diff        = n_claude - n_hand,
    count_match = n_claude == n_hand
  ) %>%
  arrange(doi)


included_comparison <- count_comparison %>% filter(n_hand > 0) %>% 
  mutate(same = (diff == 0),
         claude_more = (diff > 0),
         hand_more = (diff < 0),
         total_claude = sum(n_claude),
         total_hand = sum(n_hand))

included_comparison

table(included_comparison$same)
table(included_comparison$hand_more)
table(included_comparison$claude_more)


# check which articles hand excluded and if claude also excluded them
hand_exclude <- hand_extract %>% 
  filter(exclude == "yes" | is.na(auc)) %>% 
  distinct(doi)

claude_exclude <- claude_extract %>% 
  filter(exclude == "yes" | is.na(auc)) %>% 
  distinct(doi)

table(hand_exclude$doi %in% claude_exclude$doi)





# compare the AUC values between hand and claude
# Restrict to DOIs present in hand_extract
target_dois <- hand_extract %>% filter(auc != "na")
target_dois <- unique(target_dois$doi)

# get agreement between hand and claude for AUC value
# Restrict both extracts to the target DOIs
claude_sub <- claude_extract %>% filter(doi %in% target_dois) #%>% filter(!doi %in% c("10_1371_journal_pmed_1004665", "10_1002_mp_17672", "10_2106_jbjs_24_01601"))
hand_sub   <- hand_extract   %>% filter(doi %in% target_dois) #%>% filter(!doi %in% c("10_1371_journal_pmed_1004665", "10_1002_mp_17672", "10_2106_jbjs_24_01601"))

# Optional sanity check — which target DOIs are missing from each side?
setdiff(target_dois, claude_sub$doi)  # in target but not in claude_extract
setdiff(target_dois, hand_sub$doi)    # in target but not in hand_extract

# Mark presence in each extractor's set
claude_flag <- claude_sub %>%
  group_by(doi, auc) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

hand_flag <- hand_sub %>%
  group_by(doi, auc) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

# Union of all (doi, auc) pairs seen by either extractor (within target_dois)
auc_agreement_df <- full_join(
  claude_flag %>% select(doi, auc, occ) %>% mutate(claude = "present"),
  hand_flag   %>% select(doi, auc, occ) %>% mutate(hand   = "present"),
  by = c("doi", "auc", "occ")
) %>%
  mutate(
    claude = ifelse(is.na(claude), "missing", claude),
    hand   = ifelse(is.na(hand),   "missing", hand),
    agreement = case_when(
      claude == "present" & hand == "present" ~ "both",
      claude == "present" & hand == "missing" ~ "claude_only",
      hand   == "present" & claude == "missing" ~ "hand_only"
    )
  )

# Inspect
auc_agreement_df %>% arrange(doi, auc) %>% print(n = Inf)

# Summary: where do they disagree?
auc_agreement_df %>%
  count(claude, hand) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# GWET AC1
auc_agreement <- gwet.ac1.raw(
  ratings  = auc_agreement_df %>% select(claude, hand),
  weights  = "unweighted",
  conflev  = 0.95
)

# check the agreement for AUC
auc_agreement



# create a dataframe to check any differences between caldue and hand extraction
# flag the articles which calude and hand had more auc
auc_hand_more <- auc_agreement_df %>% filter(hand == "present" & claude == "missing") %>% filter(!is.na(auc))
auc_claude_more <- auc_agreement_df %>% filter(claude == "present" & hand == "missing") %>% filter(!is.na(auc))

# 10 random unique DOIs from auc_hand_more
set.seed(42)  # for reproducibility — remove or change if you want different samples each run

hand_sample_auc <- slice_sample(auc_hand_more, n = 50)

# 10 random unique DOIs from auc_claude_more
claude_sample_auc <- slice_sample(auc_claude_more, n = 50)

auc_extra <- rbind(hand_sample_auc,
                  claude_sample_auc) %>% 
  select(doi, auc, claude, hand, agreement)

# save to check by hand
write.csv(auc_extra, "02_data/auc_extra.csv")












# complete agreement and check for differences in the sensitivity value
# Mark presence in each extractor's set
claude_flag_sens <- claude_sub %>%
  group_by(doi, sens) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

hand_flag_sens <- hand_sub %>%
  group_by(doi, sens) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

# Union of all (doi, sens) pairs seen by either extractor (within target_dois)
sens_agreement_df <- full_join(
  claude_flag_sens %>% select(doi, sens, occ) %>% mutate(claude = "present"),
  hand_flag_sens   %>% select(doi, sens, occ) %>% mutate(hand   = "present"),
  by = c("doi", "sens", "occ")
) %>%
  mutate(
    claude = ifelse(is.na(claude), "missing", claude),
    hand   = ifelse(is.na(hand),   "missing", hand),
    agreement = case_when(
      claude == "present" & hand == "present" ~ "both",
      claude == "present"                     ~ "claude_only",
      hand   == "present"                     ~ "hand_only",
      hand   == "missing" & claude == "missing" ~ "both"
    )
  )

# Inspect
sens_agreement_df %>% arrange(doi, sens) %>% print(n = Inf)

# Summary: where do they disagree?
sens_agreement_df %>%
  count(claude, hand) %>%
  mutate(pct = round(100 * n / sum(n), 1))

na.omit(sens_agreement_df)

table(na.omit(sens_agreement_df)$agreement)
table(sens_agreement_df$claude)
table(sens_agreement_df$hand)

# GWET AC1
sens_agreement <- gwet.ac1.raw(
  ratings  = sens_agreement_df %>% select(claude, hand),
  weights  = "unweighted",
  conflev  = 0.95
)

# check the agreement for sens
sens_agreement



# create a dataframe to check any differences between caldue and hand extraction
# flag the articles which calude and hand had more sens
sens_hand_more <- sens_agreement_df %>% filter(hand == "present" & claude == "missing") %>% filter(!is.na(sens))
sens_claude_more <- sens_agreement_df %>% filter(claude == "present" & hand == "missing") %>% filter(!is.na(sens))

# 10 random unique DOIs from sens_hand_more
set.seed(42)  # for reproducibility — remove or change if you want different samples each run

hand_sample_sens <- slice_sample(sens_hand_more, n = 50)

# 10 random unique DOIs from sens_claude_more
claude_sample_sens <- slice_sample(sens_claude_more, n = 50)

sens_extra <- rbind(hand_sample_sens,
                   claude_sample_sens) %>% 
  select(doi, sens, claude, hand, agreement)

# save to check by hand
write.csv(sens_extra, "02_data/sens_extra.csv")











# complete agreement and check for differences in the specificity value
# Mark presence in each extractor's set
claude_flag_spec <- claude_sub %>%
  group_by(doi, spec) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

hand_flag_spec <- hand_sub %>%
  group_by(doi, spec) %>%
  mutate(occ = row_number()) %>% 
  ungroup()

# Union of all (doi, spec) pairs seen by either extractor (within target_dois)
spec_agreement_df <- full_join(
  claude_flag_spec %>% select(doi, spec, occ) %>% mutate(claude = "present"),
  hand_flag_spec   %>% select(doi, spec, occ) %>% mutate(hand   = "present"),
  by = c("doi", "spec", "occ")
) %>%
  mutate(
    claude = ifelse(is.na(claude), "missing", claude),
    hand   = ifelse(is.na(hand),   "missing", hand),
    agreement = case_when(
      claude == "present" & hand == "present" ~ "both",
      claude == "present"                     ~ "claude_only",
      hand   == "present"                     ~ "hand_only"
    )
  )

# Inspect
spec_agreement_df %>% arrange(doi, spec) %>% print(n = Inf)

spec_agreement_df

table(spec_agreement_df$agreement)
table(spec_agreement_df$claude)
table(spec_agreement_df$hand)

# Summary: where do they disagree?
spec_agreement_df %>%
  count(claude, hand) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# GWET AC1
spec_agreement <- gwet.ac1.raw(
  ratings  = spec_agreement_df %>% select(claude, hand),
  weights  = "unweighted",
  conflev  = 0.95
)

spec_agreement

# check the agreement for spec
na.omit(spec_agreement_df)

table(na.omit(spec_agreement_df)$agreement)
table(spec_agreement_df$claude)
table(spec_agreement_df$hand)


# create a dataframe to check any differences between caldue and hand extraction
# flag the articles which calude and hand had more spec
spec_hand_more <- spec_agreement_df %>% filter(hand == "present" & claude == "missing") %>% filter(!is.na(spec))
spec_claude_more <- spec_agreement_df %>% filter(claude == "present" & hand == "missing") %>% filter(!is.na(spec))

# 10 random unique DOIs from spec_hand_more
set.seed(42)  # for reproducibility — remove or change if you want different samples each run

hand_sample_spec <- slice_sample(spec_hand_more, n = 50)

# 10 random unique DOIs from spec_claude_more
claude_sample_spec <- slice_sample(spec_claude_more, n = 50)

spec_extra <- rbind(hand_sample_spec,
                    claude_sample_spec) %>% 
  select(doi, spec, claude, hand, agreement)

# save to check by hand
write.csv(spec_extra, "02_data/spec_extra.csv")





# check where hand and claude extraction differed for AUC
auc_checked <- read.csv("02_data/auc_extra_checked.csv")

auc_checked %>% 
  group_by(agreement, location, in_article) %>% 
  summarise(n = n(), .groups = "drop")


# check where hand and claude extraction differed for SENS
sens_checked <- read.csv("02_data/sens_extra_checked.csv")

sens_checked %>% 
  group_by(agreement, location, in_article) %>% 
  summarise(n = n(), .groups = "drop")


# check where hand and claude extraction differed for SPEC
spec_checked <- read.csv("02_data/spec_extra_checked.csv")

spec_checked %>% 
  group_by(agreement, location, in_article) %>% 
  summarise(n = n(), .groups = "drop")


# output a final dataframe to be plotted as data
plot_data <- claude_extract %>% 
  filter(!doi %in% hand_sub$doi | exclude == "yes")

write.csv(plot_data, file = "02_data/plot_data.csv")

