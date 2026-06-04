# 10_analysis.R

# libraries
library(tidyverse)
library(splines)
library(patchwork)
library(DescTools)

# import the cleaned data from 09_claude_validate.R
plot_data <- read.csv(file = "02_data/plot_data.csv")


# get summary data
# number of articles
length(unique(plot_data$doi))

# number of auc values
plot_data %>% 
  filter(auc != "NA") %>% 
  nrow()

# number of articles that reported a sensitivity value
plot_data %>% filter(sens != "NA") %>% 
  distinct(doi)
# number of sensitivity values
plot_data %>% filter(sens != "NA") %>% nrow()

# number of articles that reported a specificity value
plot_data %>% filter(spec != "NA") %>% 
  distinct(doi)
# number of sensitivity values
plot_data %>% filter(spec != "NA") %>% nrow()


# check if any AUC values were outside of the confidence intervales
auc_ci_check <- plot_data %>% filter(!is.na(auc_ci_low) & !is.na(auc_ci_upper)) %>% 
  mutate(ci_check = auc < auc_ci_upper & auc > auc_ci_low)

# see how many articles reported both lower and upper CI intervals
length(unique(auc_ci_check$doi))

# see how many auc values see how many auc val# see how many auc values were outside of the confidence intervals
table(auc_ci_check$ci_check)




# AUC in single units
auc_to_plot = mutate(plot_data, 
                 auc = as.numeric(auc),
                 auc_cut = cut(auc, breaks = seq(0,1,0.01)),
                 num = as.numeric(auc_cut)) %>%
  group_by(auc_cut, num) %>%
  tally() %>%
  ungroup()


# Fit the three models
fit2 <- glm(n ~ ns(num, df = 2), data = auc_to_plot, family = poisson)
fit3 <- glm(n ~ ns(num, df = 3), data = auc_to_plot, family = poisson)
fit4 <- glm(n ~ ns(num, df = 4), data = auc_to_plot, family = poisson)

# Build a prediction grid
pred_grid <- data.frame(num = seq(50, 100, length.out = 200))
pred_grid$df2 <- predict(fit2, newdata = pred_grid, type = "response")
pred_grid$df3 <- predict(fit3, newdata = pred_grid, type = "response")
pred_grid$df4 <- predict(fit4, newdata = pred_grid, type = "response")

# check the fit of the three splines
round(AIC(fit2, fit3, fit4), digits = 1)

# Reshape to long format so we get a legend
pred_long <- pivot_longer(pred_grid, cols = c(df2, df3, df4),
                          names_to = "df", values_to = "fit")

# make the auc distribution plot
auc_plot <- ggplot(data = auc_to_plot, aes(x = num, y = n)) +
  geom_bar(aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = 'identity', width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_line(data = pred_grid, aes(x = num, y = df4), linewidth = 1, linetype = "dashed") +
  xlab('AUC Value') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(49, 101),
                     breaks = c(50, 60, 70, 80, 90, 100),
                     labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(limits = c(0, 350))+
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# check the plot
auc_plot

# save the plot
ggsave(plot = auc_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/auc_distribution.jpg")



# calculate and plot the residuals
# Get residuals - several types available
auc_res_to_plot <- auc_to_plot %>%
  mutate(
    fitted = predict(fit4, newdata = ., type = "response"),
    resid_raw = n - fitted)

# Bar plot of residuals
resid_plot <- ggplot(auc_res_to_plot, aes(x = num, y = resid_raw)) +
  geom_bar(aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = "identity", width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  xlab("AUC Value") +
  ylab("Residual (observed - expected") +
  scale_x_continuous(limits = c(49, 101),
                     breaks = c(50, 60, 70, 80, 90, 100),
                     labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(limits = c(-80, 80)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
resid_plot

# save the plot
ggsave(plot = resid_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/auc_residuals.jpg")










# now for the data that is here plot the sensitivity in a distribution
# sens in single units
sens_to_plot = mutate(plot_data, 
                     sens = as.numeric(sens),
                     sens_cut = cut(sens, breaks = seq(0,100,1)),
                     num = as.numeric(sens_cut)) %>%
  group_by(sens_cut, num) %>%
  tally() %>%
  ungroup()


# Fit the three models
sens_fit2 <- glm(n ~ ns(num, df = 2), data = sens_to_plot, family = poisson)
sens_fit3 <- glm(n ~ ns(num, df = 3), data = sens_to_plot, family = poisson)
sens_fit4 <- glm(n ~ ns(num, df = 4), data = sens_to_plot, family = poisson)
sens_fit5 <- glm(n ~ ns(num, df = 5), data = sens_to_plot, family = poisson)
sens_fit6 <- glm(n ~ ns(num, df = 6), data = sens_to_plot, family = poisson)

# Build a prediction grid
sens_pred_grid <- data.frame(num = seq(0, 100, length.out = 200))
sens_pred_grid$df2 <- predict(sens_fit2, newdata = sens_pred_grid, type = "response")
sens_pred_grid$df3 <- predict(sens_fit3, newdata = sens_pred_grid, type = "response")
sens_pred_grid$df4 <- predict(sens_fit4, newdata = sens_pred_grid, type = "response")
sens_pred_grid$df5 <- predict(sens_fit5, newdata = sens_pred_grid, type = "response")
sens_pred_grid$df6 <- predict(sens_fit6, newdata = sens_pred_grid, type = "response")

# check AIC 
round(AIC(sens_fit2, sens_fit3, sens_fit4, sens_fit5, sens_fit6), digits = 1) # df4 is best fit


# Reshape to long format so we get a legend
sens_pred_long <- pivot_longer(sens_pred_grid, cols = c(df2, df3, df4),
                          names_to = "df", values_to = "fit")

# create the distribution for the sensitivity
sens_plot <- ggplot(data = sens_to_plot, aes(x = num, y = n)) +
  geom_bar(aes(fill = num %in% c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
           stat = 'identity', width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_line(data = sens_pred_grid, aes(x = num, y = df5), linewidth = 1, linetype = "dashed") +
  xlab('Sensitivity Value') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(0, 101),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 250)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
sens_plot

# save the plot
ggsave(plot = sens_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/sens_distribution.jpg")




# calculate and plot the residuals
# Get residuals - several types available
sens_res_to_plot <- sens_to_plot %>%
  mutate(
    fitted = predict(sens_fit5, newdata = ., type = "response"),
    resid_raw = n - fitted)

# Bar plot of residuals
sens_resid_plot <- ggplot(sens_res_to_plot, aes(x = num, y = resid_raw)) +
  geom_bar(aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = "identity", width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  xlab("Sensitivity Value") +
  ylab("Residual (observed - expected") +
  scale_x_continuous(limits = c(0, 101),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(-150, 150)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
sens_resid_plot

# save the plot
ggsave(plot = sens_resid_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/sens_residuals.jpg")










# now for the data that is here plot the specifciity in a distribution
# spec in single units
spec_to_plot = mutate(plot_data, 
                      spec = as.numeric(spec),
                      spec_cut = cut(spec, breaks = seq(0,100,1)),
                      num = as.numeric(spec_cut)) %>%
  group_by(spec_cut, num) %>%
  tally() %>%
  ungroup()


# Fit the three models
spec_fit2 <- glm(n ~ ns(num, df = 2), data = spec_to_plot, family = poisson)
spec_fit3 <- glm(n ~ ns(num, df = 3), data = spec_to_plot, family = poisson)
spec_fit4 <- glm(n ~ ns(num, df = 4), data = spec_to_plot, family = poisson)
spec_fit5 <- glm(n ~ ns(num, df = 5), data = spec_to_plot, family = poisson)
spec_fit6 <- glm(n ~ ns(num, df = 6), data = spec_to_plot, family = poisson)

# Build a prediction grid
spec_pred_grid <- data.frame(num = seq(0, 100, length.out = 200))
spec_pred_grid$df2 <- predict(spec_fit2, newdata = spec_pred_grid, type = "response")
spec_pred_grid$df3 <- predict(spec_fit3, newdata = spec_pred_grid, type = "response")
spec_pred_grid$df4 <- predict(spec_fit4, newdata = spec_pred_grid, type = "response")
spec_pred_grid$df5 <- predict(spec_fit5, newdata = spec_pred_grid, type = "response")
spec_pred_grid$df6 <- predict(spec_fit6, newdata = spec_pred_grid, type = "response")

# check AIC 
round(AIC(spec_fit2, spec_fit3, spec_fit4, spec_fit5, spec_fit6), digits = 1) # df4 is best fit


# Reshape to long format so we get a legend
spec_pred_long <- pivot_longer(spec_pred_grid, cols = c(df2, df3, df4),
                               names_to = "df", values_to = "fit")

# create the distribution for the specitivity
spec_plot <- ggplot(data = spec_to_plot, aes(x = num, y = n)) +
  geom_bar(aes(fill = num %in% c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
           stat = 'identity', width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_line(data = spec_pred_grid, aes(x = num, y = df4), linewidth = 1, linetype = "dashed") +
  xlab('Specificity Value') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(0, 101),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 250)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
spec_plot

# save the plot
ggsave(plot = spec_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/spec_distribution.jpg")



# calculate and plot the residuals
# Get residuals - several types available
spec_res_to_plot <- spec_to_plot %>%
  mutate(
    fitted = predict(spec_fit4, newdata = ., type = "response"),
    resid_raw = n - fitted)

# Bar plot of residuals
spec_resid_plot <- ggplot(spec_res_to_plot, aes(x = num, y = resid_raw)) +
  geom_bar(aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = "identity", width = 1, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "lightblue"), guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  xlab("Specificity Value") +
  ylab("Residual (observed - expected") +
  scale_x_continuous(limits = c(0, 101),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(-150, 150)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
spec_resid_plot

# save the plot
ggsave(plot = spec_resid_plot,
       width = 12,
       height = 10,
       dpi = 500,
       filename = "03_figures/spec_residuals.jpg")




# combine the plots for values into three seperate plots

auc_final <- (auc_plot + labs(tag = "A")) / (resid_plot + labs(tag = "B"))
sens_final <- (sens_plot + labs(tag = "A")) / (sens_resid_plot + labs(tag = "B"))
spec_final <- (spec_plot + labs(tag = "A")) / (spec_resid_plot + labs(tag = "B"))

ggsave(auc_final, file = "03_figures/auc_final.jpg",
       width = 8,
       height = 10,
       dpi = 500)

ggsave(sens_final, file = "03_figures/sens_final.jpg",
       width = 8,
       height = 10,
       dpi = 500)

ggsave(sens_final, file = "03_figures/spec_final.jpg",
       width = 8,
       height = 10,
       dpi = 500)



# get the mean sensitivity and specificity values
mean(na.omit(plot_data$sens))
mean(na.omit(plot_data$spec))


# plot the sample sizes
cumulative_data <- plot_data %>%
  select(sample_size) %>% 
  na.omit() %>% 
  count(sample_size) %>%
  arrange(sample_size) %>%
  mutate(
    pct = n / sum(n) * 100,
    cumulative_pct = cumsum(pct)
  ) %>%
  # Ensure x-axis starts at 0
  bind_rows(tibble(sample_size = 0, n = 0, pct = 0, cumulative_pct = 0), .) %>%
  arrange(sample_size)


fifty_ss <- cumulative_data$sample_size[which.min(abs(cumulative_data$cumulative_pct - 50))]
ninty_ss <- cumulative_data$sample_size[which.min(abs(cumulative_data$cumulative_pct - 90))]

cumulative_sample_size <- cumulative_data %>%
  ggplot(aes(x = log(sample_size), y = cumulative_pct))+
  geom_line()+
  labs(x = "Sample Size (Log Scale)",
       y = "Proportion of Models")+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"))+
  annotate("segment", x = log(fifty_ss), xend = log(fifty_ss),
           y = 0, yend = 50,
           linetype = "dashed", color = "black")+
  annotate("segment", x = min(log(cumulative_data$sample_size)), xend = log(fifty_ss),
           y = 50, yend = 50,
           linetype = "dashed", color = "black")+
  annotate("segment", x = log(ninty_ss), xend = log(ninty_ss),
           y = 0, yend = 90,
           linetype = "dashed", color = "black")+
  annotate("segment", x = min(log(cumulative_data$sample_size)), xend = log(ninty_ss),
           y = 90, yend = 90,
           linetype = "dashed", color = "black")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

ggsave(cumulative_sample_size, file = "03_figures/cumulative_sample_size.jpg",
       width = 6,
       height = 4,
       dpi = 500)

# gather the summary data of the articles and models

min(na.omit(plot_data$sample_size))
max(na.omit(plot_data$sample_size))
median(na.omit(plot_data$sample_size))
Mode(na.omit(plot_data$sample_size))

# check the % diag and prog
plot_data %>%
  group_by(model_type) %>% 
  summarise(n = n())

# check the % development and validation
# 18 models were input as prognostic 
# checked and models are developmental
plot_data %>%
  group_by(study_type) %>% 
  summarise(n = n())

