# analysis_pilot.R
# complete an analysis on the pilot studies

# load in libraries
library(tidyverse)
library(readxl)

# load in data
df <- read.csv(file = "02_data/02_pilot/article_data_pilot.csv")

# gather summary statistics



# number of articles and number of models
df %>% 
  summarise(total_articles = length(unique(article)),
            total_models = length(model),
            total_diag = sum(model_type == "diagnostic"),
            percent_diag = round((sum(model_type == "diagnostic")/(length(df$model))),digits = 2)*100,
            total_prog = sum(model_type == "prognostic"),
            percent_prog = round((sum(model_type == "prognostic")/(length(df$model))),digits = 2)*100,
            total_development = sum(study_type == "development"),
            percent_dev = round((sum(study_type == "development")/(length(df$model))),digits = 2)*100,
            total_validation = sum(study_type == "validation"),
            percent_val = round((sum(study_type == "validation")/(length(df$model))),digits = 2)*100,)


# number of articles that included a sample size
df %>% 
  summarise(inc_sample = sum(sample_size != "na"),
            mean_sample = round(mean(sample_size)),
            sd_sample = round(sd(sample_size)),
            roc = sum(roc == "yes"),
            percent_roc = sum(roc == "yes")/length(df$model), # need to figure out what is going on here to get the % of roc inclusion
            mean_models = round(length(model)/length(unique(article)), digits = 2))





# Summary stats for AUC value, total, %, mean, SD
df %>% filter(auc != 0) %>% 
  summarise(percent = round(nrow(.)/length(df$model), digits = 2)*100,
            total = round(nrow(.)),
            mean = round(mean(as.numeric(auc)), digits = 2),
            SD = round(sd(auc), digits = 2))

# summary stats for sens value, total, %, mean, SD
df %>% filter(sens != "na") %>% 
  summarise(percent = round(nrow(.)/length(df$model), digits = 2)*100,
            total = nrow(.),
            mean = round(mean(as.numeric(sens)), digits = 2),
            SD = round(sd(as.numeric(sens)), digits = 2))

# summary stats for spec value, total, %, mean, SD
df %>% filter(spec != "na") %>% 
  summarise(percent = round(nrow(.)/length(df$model), digits = 2)*100,,
            total = nrow(.),
            mean = round(mean(as.numeric(spec)), digits = 2),
            SD = round(sd(as.numeric(spec)), digits = 2))




# plot the distribution of AUC values
df %>% 
  filter(auc != "na") %>% 
  ggplot(aes(x = auc))+
  geom_histogram(binwidth = 0.05, colour = "black", fill = "grey60")+
  theme_classic()+
  labs(x = "AUC Values",
       y = "Count")

# save the histogram plot for appendix
ggsave(filename = "03_figures/histogram_auc_pilot.png",
       height = 6,
       width = 8,
       dpi = 300)


# plot the distribution of sensitivity values
df %>% 
  filter(sens != "na") %>% 
  ggplot(aes(x = as.numeric(sens)))+
  geom_histogram(binwidth = 0.05, colour = "black", fill = "grey60")+
  theme_classic()+
  labs(x = "Sensitivity Values",
       y = "Count")


# save the histogram plot for appendix
ggsave(filename = "03_figures/histogram_sens_pilot.png",
       height = 6,
       width = 8,
       dpi = 300)



# plot the distribution of specificity values
df %>% 
  filter(spec != "na") %>% 
  ggplot(aes(x = as.numeric(spec)))+
  geom_histogram(binwidth = 0.05, colour = "black", fill = "grey60")+
  theme_classic()+
  labs(x = "Specificity Values",
       y = "Count")


# save the histogram plot for appendix
ggsave(filename = "03_figures/histogram_spec_pilot.png",
       height = 6,
       width = 8,
       dpi = 300)


# check for the articles which included AUC values and CI that the estimate is between the bounds
df %>% 
  filter(auc != "na" & auc_lower != "na" & auc_upper != "na") %>% 
  nrow()

df %>%
  filter(auc != "na" & auc_lower != "na" & auc_upper != "na") %>% 
  filter(auc > auc_lower & auc < auc_upper) %>% 
  nrow()


# check for the articles which included sensitivity values and CI that the estimate is between the bounds
df %>% 
  filter(sens != "na" & sens_lower != "na" & sens_upper != "na") %>% 
  nrow()

df %>%
  filter(sens != "na" & sens_lower != "na" & sens_upper != "na") %>% 
  filter(sens > sens_lower & auc < sens_upper) %>% 
  nrow()

# check for the articles which included specificity values and CI that the estimate is between the bounds
df %>% 
  filter(spec != "na" & spec_lower != "na" & spec_upper != "na") %>% 
  nrow()

df %>%
  filter(spec != "na" & spec_lower != "na" & spec_upper != "na") %>% 
  filter(spec > spec_lower & auc < spec_upper) %>% 
  nrow()
