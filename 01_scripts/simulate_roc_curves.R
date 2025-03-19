# simulate ROC curves for pilot extraction
# created February 2025

# load libraries
library(ggplot2)
library(plotROC)

# set a seed for reproducible results
set.seed(3625)

# create a list of 50 values ranging from 10 to 5000 for different sample sizes
sample <- sample(10:1000,50)

# create an empty list to store values
outcomes <- list()

# set a binary outcome variable for disease or non-disease
for(i in 1:length(sample)){
  outcome <- rbinom(sample[i], size = 1, prob = .5)
  tmp <- list(outcome)
  outcomes[i] <- tmp
}

# set the range of SD
sd <- runif(n = 50, min = 0.5, max = 2)

# create an empty list to store data frames
outcomes_df_list <- list()

# generate data for each sample size
for(i in 1:length(sample)){
  
  # binary outcome variable for disease or non-disease
  outcome <- rbinom(sample[i], size = 1, prob = .5)
  
  # continuous variable with a random standard deviation from the 'sds' vector
  var1 <- rnorm(sample[i], mean = outcome, sd = sd[i])
  
  # create a data frame for ROC analysis
  roc_df <- data.frame(
    outcome = outcome,
    outcome_id = c("disease", "non-disease")[outcome + 1],  # Convert 0/1 to "disease"/"non-disease"
    var = var1,
    stringsAsFactors = FALSE
  )
  
  # store the data frame in the list
  outcomes_df_list[[i]] <- roc_df
}

#create a list for all 50 plots
plot_list <- list()

# for each date frame make the plot
for (i in 1:length(outcomes_df_list)){
  plot <- ggplot(outcomes_df_list[[i]], aes(d = outcome, m = var)) + geom_roc(n.cuts = 0, labels = FALSE)
  plot_list[i] <- list(plot)
}

# get the auc value of each of the roc curves
# set an empty list to store auc calc
auc_list <- list()

# for each plot take the auc and add to the list
for(i in 1:length(plot_list)){
  auc <- calc_auc(plot_list[[i]])
  auc_list[i] <- c(list(auc))
}

# from the list take all of the auc values and put into data frame
# set an empty dataframe
auc_df <- data.frame()

# iterate through and take just the auc value
for (i in 1:length(auc_list)){
  auc_int <- auc_list[[i]][[3]]
  auc_df <- rbind(auc_df, as.numeric(auc_int))
}

# rename the auc_df variable
colnames(auc_df) <- c("auc")

# go through the list of plots and save each plot
# set the first plot number
plot_num <- 1

# iterate and save each plot
for (i in 1:length(plot_list)){
  file_name <- paste("03_figures/roc_sim_figures/sim_roc", plot_num, ".jpg", sep = "")
  ggsave(plot_list[[i]],
         filename = file_name)
  plot_num <- plot_num + 1
}






