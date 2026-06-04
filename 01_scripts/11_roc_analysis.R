# 11_roc_analysis.R

# libraries
library(tidyverse)
library(bayestestR)

# import the data that met inclusion criteria
plot_data <- read.csv(file = "02_data/plot_data.csv")

# filter the data to only be those whith ROC curves identified
rocs <- plot_data %>% 
  filter(roc == "yes")

# sort the articles into a random list
# then manually go through and check if the roc curve and auc correspond
# if so they download the ROC curve for extraction

# # set a seed for reproducible
# set.seed(2001)
# 
# # organise them into the list
# rocs_rand <- rocs[sample(nrow(rocs)), ]

## save the random list to gather roc curves manually
# write.csv(rocs_rand, file = "02_data/rocs_rand.csv")

# import the checked rocs to ensure 200 are to be extracted
rocs_rand_checked <- read.csv(file = "02_data/rocs_rand_checked.csv")

# check there is 200 ROC curves
rocs_rand_checked %>% filter(had_roc == "yes" & auc_roc_article_match == "yes") %>% nrow()

# filter a dataframe of only the 200 ROC to extract from
rocs_extract_from <- rocs_rand_checked %>% 
  filter(had_roc == "yes" & auc_roc_article_match == "yes") %>% 
  select(doi, auc, true_colour, true_panel) %>%
  group_by(doi) %>%
  mutate(doi = case_when(
    row_number() == 2 ~ paste0(doi, "_two"),
    row_number() == 3 ~ paste0(doi, "_three"),
    TRUE ~ doi
  )) %>%
  ungroup()

# save the data
# write.csv(rocs_extract_from, file = "02_data/rocs_extract.csv" )















# read in the extracted roc curves


# calculate the auc from the extracted data

# calculate the difference from the extracted and reported (reported - extracted)

# plot a histogram of the differences


# Get file list with names
temp <- list.files(
  path = "02_data/05_roc_completed/01_extracted",
  pattern = "\\.csv$",
  ignore.case = TRUE,
  full.names = TRUE
)

# Extract DOI from filename (strip path and .csv extension)
file_dois <- tools::file_path_sans_ext(basename(temp))

# Import CSVs as named list, keyed by DOI
files <- setNames(lapply(temp, read.csv), file_dois)

derived_auc_df <- do.call(rbind, lapply(file_dois, function(doi) {
  df <- files[[doi]]
  
  # Sort by x (FPR) ascending before calculating AUC
  df <- df[order(df$x), ]
  
  auc_val <- area_under_curve(x = df$x, y = df$y, method = "trapezoid")
  data.frame(doi = doi, derived_auc = auc_val, stringsAsFactors = FALSE)
}))

# Join to reported AUC by DOI
roc_auc <- merge(
  rocs_extract_from[, c("doi", "auc")],
  derived_auc_df,
  by = "doi",
  all.x = FALSE,
  all.y = TRUE
)

# Calculate difference
roc_auc$auc_diff <- roc_auc$auc - roc_auc$derived_auc

# order them by most difference to least
roc_auc %>% arrange(desc(auc_diff))

# plot the data in a histogram
labels_df <- data.frame(direction = c("Negative", "Positive"),
                        label = c("Derived AUC higher", "Reported AUC higher"),
                        x = c(-0.1, 0.1),
                        y = c(75, 75))

auc_diff_plot <- roc_auc %>%
  mutate(direction = ifelse(auc_diff < 0, "Negative", "Positive")) %>%
  ggplot(aes(x = auc_diff))+
  geom_histogram(binwidth = 0.005, boundary = 0, colour = "black", fill = "lightblue") +
  geom_text(data = labels_df,
            aes(x = x, y = y, label = label),
            size = 5,
            inherit.aes = FALSE)+
  scale_x_continuous(breaks = c(-0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2),
                     limits = c(-0.2, 0.2))+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank())+
  labs(x = "AUC Difference (Reported - Derived)", y = "Count") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ ifelse(auc_diff < 0, "Negative", "Positive"),
             scales = "free_x",
             nrow = 1)+
  facetted_pos_scales(x = list(Negative = scale_x_continuous(limits = c(-0.2, 0),
                                                             breaks = c(-0.2, -0.15, -0.1, -0.05, 0)),
                               Positive = scale_x_continuous(limits = c(0, 0.2),
                                                             breaks = c(0, 0.05, 0.1, 0.15, 0.2))))

auc_diff_plot

# save the plot
ggsave(plot = auc_diff_plot,
       width = 12,
       height = 8,
       dpi = 500,
       filename = "03_figures/auc_diff.jpg")


# check the articles where the difference between reporting and derived is greater 0.01
roc_auc %>% filter(auc_diff > 0.01) %>% arrange(auc_diff)
roc_auc %>% filter(auc_diff < -0.01) %>% arrange(auc_diff)

  
# check any images that were missed
# list of images that could not be extracted
non_extract <- c("10_1007_s00415-025-13361-0", "10_1016_j_injury_2025_112934", "10_1080_07853890_2025_2566869", "10_3389_fmed_2025_1606336_two",
                 "10_1002_ccd_31498", "10_2147_clep_s566997", "10_2147_tcrm_s504745", "10_1016_j_exger_2025_112784", "10_1007_s00062-025-01593-6",
                 "10_1007_s11547-025-01949-5_three", "10_3389_fendo_2025_1593917_two")

# check the missing
missing <- rocs_extract_from$doi[!rocs_extract_from$doi %in% file_dois & 
                                   !rocs_extract_from$doi %in% non_extract]


# get the mean AUC value difference
mean(roc_auc$auc_diff)
sd(roc_auc$auc_diff)

# check how many above and below
roc_auc %>% filter(auc_diff > 0) %>% nrow()
roc_auc %>% filter(auc_diff < 0) %>% nrow()




















auc_diff_plot
