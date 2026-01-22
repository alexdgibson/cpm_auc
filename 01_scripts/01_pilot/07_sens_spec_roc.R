# sens_spec_roc.R
# code to check if the sensitivity and specificity lay on the ROC curve


library(ggplot2)
library(purrr)

df2 <- df %>% 
  filter(sens != "na" & spec != "na") %>% 
  filter(roc == "yes") %>% 
  mutate(
    sens = as.numeric(sens),
    spec = as.numeric(spec),
    x_reported = 1 - spec
  )


roc_list <- tibble(
  article = file_info$article,
  model   = file_info$model,
  roc_df  = files
)

roc_full <- df2 %>%
  left_join(roc_list, by = c("article", "model"))




walk(1:nrow(roc_full), function(i) {
  
  row <- roc_full[i, ]
  roc_df <- row$roc_df[[1]]
  
  p <- ggplot(roc_df, aes(x = x, y = y)) +
    geom_line(color = "black", size = 1) +
    geom_point(aes(x = row$x_reported, y = row$sens),
               color = "red",
               size = 3)+
    labs(title = paste0("ROC Curve — Article ", row$article, ", Model ", row$model),
         subtitle = paste0("Reported sens = ", row$sens,
                           ", spec = ", row$spec,
                           "(x = ", round(row$x_reported, 3), ")"),
         x = "1 - Specificity",
         y = "Sensitivity")+
    theme_classic(base_size = 14)
  
  ggsave(
    filename = paste0("03_figures/pilot_sens_spec/roc_article_", 
                      row$article, "_model_", row$model, ".png"),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
})
