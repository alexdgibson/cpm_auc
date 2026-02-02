# article_extract_auc_roc.R
# comparing the extracted AUC value from ROC article with reported

# extract AUC values from ROC graph images
# February 2025

# load libraries
library(tidyverse)
library(bayestestR)

# read in the AUC values from the articles along with DOI
df <- read.csv("02_data/02_pilot/article_data_pilot.csv")

# load all CSV files
temp <- list.files(
  path = "./02_data/02_pilot/01_roc_extract",
  pattern = "\\.csv$",
  ignore.case = TRUE,
  full.names = TRUE
)

# sort numerically
temp_sorted <- str_sort(temp, numeric = TRUE)

# import as list of data frames
files <- lapply(temp_sorted, read.csv)

# extract article + model numbers from filenames
file_info <- tibble(
  temp = temp_sorted,
  filename = basename(temp_sorted)
) %>%
  mutate(
    # extract the trailing "_1_1" pattern
    id = str_extract(filename, "_\\d+_\\d+"),
    # remove leading underscore
    id = str_remove(id, "^_"),
    # split into article and model
    article = as.numeric(str_split(id, "_", simplify = TRUE)[,1]),
    model   = as.numeric(str_split(id, "_", simplify = TRUE)[,2])
  )

# compute AUC for each file
derived_auc_df <- map_dbl(files, ~ area_under_curve(.x$x, .x$y, method = "trapezoid")) %>%
  as.data.frame() %>%
  rename(derived_auc = ".")

# combine AUC results with extracted file info
roc_auc <- bind_cols(file_info, derived_auc_df)

# now join with df using article + model
# (assuming df also has columns named article and model)
roc_auc <- roc_auc %>%
  left_join(df, by = c("article", "model")) %>%
  mutate(diff = auc - derived_auc)


# calculate the difference between the actual auc and the extracted auc value
roc_auc <- roc_auc %>% mutate(
  #auc = round(auc, digits = 2),
  diff = auc - derived_auc)

# check the results
roc_auc %>% select(doi, auc, diff, derived_auc)

# save the outcome
save(roc_auc, file = "02_data/02_pilot/pilot_roc_auc.Rdata")

# plot the data in a histogram
roc_auc %>% 
  ggplot(aes(x = diff))+
  geom_histogram(binwidth = 0.005, boundary = 1, colour = "black", fill = "grey60")+
  # scale_x_continuous(limits = c(-0.02, 0.03))+
  theme_classic()+
  theme(text = element_text(size = 16))+
  labs(x = "Difference in AUC Values",
       y = "Count")+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  annotate("text", x = -0.0155, y = 12, label = "ROC AUC Higher")+
  annotate("text", x = 0.0155, y = 12, label = "Reported AUC Higher")



# save the histogram plot for appendix
ggsave(filename = "03_figures/histogram_roc_pilot.png",
       height = 6,
       width = 8,
       dpi = 300)

# Filter to the articles which had a difference of greater than 0.05
roc_auc %>% 
  filter(diff > 0.05 | diff < -0.05) %>% 
  select(doi, derived_auc, auc, article, model)

# get the mean, standard deviation and range of the values
mean(roc_auc$diff)
sd(roc_auc$diff)
min(roc_auc$diff)
max(roc_auc$diff)

# round the answers to 5 decimal places
round(mean(roc_auc$diff), digits = 5)
round(sd(roc_auc$diff), digits = 5)
round(min(roc_auc$diff), digits = 5)
round(max(roc_auc$diff), digits = 5)






# check if the reported sensitivity value is somewhere on the ROC curve
df %>% filter(sens != "na" & spec != "na") %>% 
  select(doi, article, model, sens, spec)
  





