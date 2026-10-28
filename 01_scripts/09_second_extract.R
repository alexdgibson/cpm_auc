# 09_second_extract.R
# extraction the second set of data

# load libraries
library(tidyverse)

# create a data frame to save any files that fail to import
failed_files <- c()

# make all imports character for any inconsistencies
safe_read <- function(file) {
  tryCatch(
    read_csv(file) |> mutate(across(everything(), as.character)),
    error = function(e) {
      failed_files <<- c(failed_files, file)
      NULL
    }
  )
}

# load in using the safe load
second_extract <- list.files("02_data/03_screening_2/02_completed/02_csv_complete/",
                 pattern = "^clinical_prediction_models_",
                 full.names = TRUE) |>
  lapply(safe_read) |>
  Filter(Negate(is.null), x = _) |>
  bind_rows()

# #save the combined files into one file
# save(second_extract, file = "02_data/second_extracted.Rdata")
load(file = "02_data/second_extracted.Rdata")


# change the values so they are the same type
# claude
second_extract$auc <- as.numeric(second_extract$auc)
second_extract$auc_ci_low <- as.numeric(second_extract$auc_ci_low)
second_extract$auc_ci_upper <- as.numeric(second_extract$auc_ci_upper)
second_extract$sample_size <- as.numeric(second_extract$sample_size)
second_extract$sens <- as.numeric(second_extract$sens)
second_extract$sens_ci_lower <- as.numeric(second_extract$sens_ci_lower)
second_extract$sens_ci_upper <- as.numeric(second_extract$sens_ci_upper)
second_extract$spec <- as.numeric(second_extract$spec)
second_extract$spec_ci_lower <- as.numeric(second_extract$spec_ci_lower)
second_extract$spec_ci_upper <- as.numeric(second_extract$spec_ci_upper)
second_extract$article <- as.numeric(second_extract$article)

# check that the data are in the same decimal (i.e., 0.778 not 77.8)
second_extract <- second_extract %>%
  mutate(sens = ifelse(sens <= 1, sens * 100, sens))

second_extract <- second_extract %>%
  mutate(sens_ci_lower = ifelse(sens_ci_lower <= 1, sens_ci_lower * 100, sens_ci_lower))

second_extract <- second_extract %>%
  mutate(sens_ci_upper = ifelse(sens_ci_upper <= 1, sens_ci_upper * 100, sens_ci_upper))

second_extract <- second_extract %>%
  mutate(spec = ifelse(spec <= 1, spec * 100, spec))

second_extract <- second_extract %>%
  mutate(spec_ci_lower = ifelse(spec_ci_lower <= 1, spec_ci_lower * 100, spec_ci_lower))

second_extract <- second_extract %>%
  mutate(spec_ci_upper = ifelse(spec_ci_upper <= 1, spec_ci_upper * 100, spec_ci_upper))

str(second_extract)

# how many articles were included for full text

second_plot_data <- second_extract %>% 
  filter(exclude == "no",
         !is.na(auc))



# write it to file
 write.csv(second_plot_data, file = "02_data/second_plot_data.csv")
 