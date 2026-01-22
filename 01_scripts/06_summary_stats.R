# 06_summary_statistics.R
# create a table of summary statistics from for the articles


# load in the data
df <- read.csv(file = "02_data/example_data/example_data.csv")

# filter data to only those which were included
# ensure to gather the info of articles which were excluded


# summarise the data
summarise(df,
          diagnostic = sum(model_type == "diagnostic"),
          prognostic = sum(model_type == "prognostic"),
          Number_Of_AUC = length(auc_value),
          Number_of_Sens = length(sens_value),
          Sumber_of_Spec = length(spec_value),
          AUC_Mean = round(mean(auc_value), digits = 2),
          Sens_Mean  = round(mean(sens_value), digits = 2),
          Spec_Mean = round(mean(spec_value), digits = 2),
          AUC_SD = round(sd(auc_value), digits = 2),
          Sens_SD = round(sd(sens_value), digits = 2),
          Spec_SD = round(sd(spec_value), digits = 2),
          
)

          