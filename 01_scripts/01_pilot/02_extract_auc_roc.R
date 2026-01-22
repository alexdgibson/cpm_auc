# extract AUC values from ROC graph images
# February 2025

# load libraries
library(tidyverse)
library(bayestestR)

# read in the AUC values from the simulated ROC curves in 01_simulate_roc_curves.R
auc_df <- readRDS(file = "02_data/auc_sim_df.RDS")

# load in all the extracted data as individual data frames in one list
# set the path for all csv files
temp <- list.files(
  path = "./02_data/01_roc_auc_validate",
  pattern = "\\.csv$",
  ignore.case = T,
  full.names = T)

# sort them by numeric not string
temp_sorted <- str_sort(temp, numeric = TRUE)

# import as the list of data frames
files <- lapply(temp_sorted, read.csv)

# calculate the AUC value from the extracted data by looping over each df in the list
# set an empty data frame to store values
derived_auc_df <- data.frame()

# loop over each data frame and calculate the AUC value and store it in the data frame
for (i in 1:length(files)){
  derived_auc <- area_under_curve(x = files[[i]]$x, files[[i]]$y, method = "trapezoid")
  derived_auc_df <- rbind(derived_auc_df, derived_auc)
}

# calculate the difference between the AUC value extracted and the real AUC value
roc_auc <- cbind(head(auc_df, 50), derived_auc_df) 

# rename both variables
colnames(roc_auc) <- c("auc", "derived_auc")

# calculate the difference between the actual auc and the extracted auc value
roc_auc <- roc_auc %>% mutate(
  #auc = round(auc, digits = 2),
  diff = auc - derived_auc)

# check the results
roc_auc

# plot the data in a histogram
roc_auc %>% 
  ggplot(aes(x = diff))+
  geom_histogram(binwidth = 0.001, boundary = 1, colour = "black", fill = "grey60")+
  scale_x_continuous(breaks = c(-0.003,-0.002,-0.001,0,0.001,0.002,0.003))+
  theme_classic()+
  theme(text = element_text(size = 16))+
  labs(x = "Difference in Value",
       y = "Count")+
  geom_vline(xintercept = 0, linetype = 'dashed')

# save the histogram plot for appendix
ggsave(filename = "03_figures/histogram_roc_validation_extraction.png",
       height = 6,
       width = 8,
       dpi = 300)

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





