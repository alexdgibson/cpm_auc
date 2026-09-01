library(dplyr)
library(tidyr)
library(lme4)
library(splines)
library(mgcv)


# import the cleaned data from 09_claude_validate.R
## will need to change the file location to import the data ##
plot_data <- read.csv(file = "02_data/plot_data.csv")


# binning and removing AUC below 0.5
auc_to_plot = mutate(plot_data, 
                     auc = as.numeric(auc),
                     auc_cut = cut(auc, breaks = seq(0,1,0.01)),
                     num = as.numeric(auc_cut),
                     doi = factor(doi)) %>%
  group_by(num) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(doi, auc, auc_cut, num, n) %>%
  filter(num >49) %>%
  na.omit()

# the rounds AUC value for each binwidth
# this is putting the AUC bins into a single numeric variable, num
num_lookup <- auc_to_plot %>%
  distinct(auc_cut, num)

# take the data with AUC values and turn it into a data frame for modelling
# final data frame consists of 101 AUC values for each variable 0.00:1.00
# 1,325 articles * 101 = 133,825 rows
# then gathers the number of times each AUC bin appears in each DOI (article level)
# then gathers the number of times each AUC bin appears for all articles (sample level)
final_df <- auc_to_plot %>%
  group_by(doi, auc_cut) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(doi, auc_cut, fill = list(n = 0)) %>%
  left_join(num_lookup, by = "auc_cut") %>%
  group_by(auc_cut) %>%
  mutate(auc_total = sum(n)) %>%
  ungroup()

# remove any data that is below 0.50 (from 0.00 to 1.00 AUC generation)
# doi is a factor of the article
# auc_cut is the bin setting
# n is the number of AUC values in the bin in the article
# num is the AUC value
# auc_total is the total number of AUC values in the bind for all articles
final_data <- final_df %>% filter(num > 49)


# fitting with the glm package and trying to work up from basic models
# fit a natural spline without random effect for DOI on article level data
fit_spline <- glm(auc_total ~ ns(num, df = 3), data = final_data, family = poisson) # has an AIC of 60,416??

# check model fit and AIC
summary(fit_spline)
AIC(fit_spline)


# create a new data frame with the num values to predict at
newdata <- final_data %>%
  distinct(num) %>%
  arrange(num)

# predict on the link scale to keep SEs -> symmetric CIs, and then back-transform
pred <- predict(fit_spline, newdata = newdata, type = "link", se.fit = TRUE)

# transform the SE to CIs 
newdata <- newdata %>%
  mutate(fit   = fit_spline$family$linkinv(pred$fit),
         lower = fit_spline$family$linkinv(pred$fit - 1.96 * pred$se.fit),
         upper = fit_spline$family$linkinv(pred$fit + 1.96 * pred$se.fit))

# gather the raw data from the initial data
auc_totals <- final_data %>% 
  group_by(num) %>% 
  select(auc_total) %>% 
  distinct(auc_total)


# plot the data raw data and the modelled data
auc_plot_1 <- ggplot(newdata, aes(x = num, y = fit)) +
  geom_bar(data = auc_totals, aes(y = auc_total, fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = 'identity', width = 1, colour = "black") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "red") +
  geom_line(color = "black", linewidth = 1, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#B52B12", "FALSE" = "#FFBDAD"), guide = "none") +
  xlab('AUC Value') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(49, 101),
                     breaks = c(50, 60, 70, 80, 90, 100),
                     labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(limits = c(0, 350))+
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
auc_plot_1







# complete now for the random effects model with DOI and a p-spline

# from the data frame above (final_data) model with a p-spline and random intercept
# using bam() over gam() as it is designed for large datasets with 10,000's of rows (see https://cran.r-project.org/web/packages/mgcv/mgcv.pdf)
fit_ri <- bam(auc_total ~ s(num, bs = "ps", k = 4) + s(doi, bs= "re"), family = poisson, data = final_data, method = "fREML", discrete = TRUE)

# check the summary fits
summary(fit_ri)
AIC(fit_ri)

# create new data frame to predict on
newdata_ri <- final_data %>%
  distinct(num) %>%
  arrange(num) %>%
  mutate(doi = factor(levels(final_data$doi)[1], levels = levels(final_data$doi)))
# doi value here is a placeholder -- it gets excluded below, so any valid level works

# predictions from the model on the AUC bins
pred <- predict(fit_ri, newdata = newdata_ri, type = "link",
                se.fit = TRUE, exclude = "s(doi)")

# transform the SEs to CIS
newdata_ri <- newdata_ri %>%
  mutate(fit   = exp(pred$fit),
         lower = exp(pred$fit - 1.96 * pred$se.fit),
         upper = exp(pred$fit + 1.96 * pred$se.fit))

# the raw observed totals of AUCs per bin
observed <- final_data %>%
  distinct(num, auc_total)

# plot the predictions of AUCs and the raw counts total
auc_plot_ri <- ggplot(newdata, aes(x = num, y = fit)) +
  geom_bar(data = observed, aes(y = auc_total, fill = num %in% c(50, 60, 70, 80, 90, 100)),
           stat = 'identity', width = 1, colour = "black") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "red") +
  geom_line(color = "black", linewidth = 1, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#B52B12", "FALSE" = "#FFBDAD"), guide = "none") +
  xlab('AUC Value') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(49, 101),
                     breaks = c(50, 60, 70, 80, 90, 100),
                     labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(limits = c(0, 350))+
  theme_bw() +
  theme(panel.grid.minor = element_blank())
  


# check the plot
auc_plot_ri
auc_plot_1








