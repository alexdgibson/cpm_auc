# 10_analysis_auc.R

# libraries
library(tidyverse)
library(splines)
library(patchwork)
library(DescTools)
library(mgcv)
library(lme4)

# import the cleaned data from 09_claude_validate.R
first_plot_data <- read.csv(file = "02_data/plot_data.csv")
second_plot_data <- read.csv(file = "02_data/second_plot_data.csv")

# bind both datasets together
plot_data <- rbind(first_plot_data, second_plot_data)

# get summary data
# number of articles
length(unique(plot_data$doi))

# number of auc values
plot_data %>%
  filter(auc != "NA") %>%
  nrow()

# number of articles that reported a sensitivity value
plot_data %>% filter(sens != "NA") %>% distinct(doi) %>% nrow()
# number of sensitivity values
plot_data %>% filter(sens != "NA") %>% nrow()

# number of articles that reported a specificity value
plot_data %>% filter(spec != "NA") %>% distinct(doi) %>% nrow()
# number of sensitivity values
plot_data %>% filter(spec != "NA") %>% nrow()


# check if any AUC values were outside of the confidence intervales
auc_ci_check <- plot_data %>%
  filter(!is.na(auc_ci_low) & !is.na(auc_ci_upper)) %>%
  mutate(ci_check = auc < auc_ci_upper & auc > auc_ci_low)

# see how many articles reported both lower and upper CI intervals
length(unique(auc_ci_check$doi))

# see how many auc values see how many auc val# see how many auc values were outside of the confidence intervals
table(auc_ci_check$ci_check)


# AUC in single units
auc_to_plot2 = mutate(
  plot_data,
  auc = as.numeric(auc),
  auc_cut = cut(auc, breaks = seq(0, 1, 0.01)),
  num = as.numeric(auc_cut)
) %>%
  group_by(auc_cut, num) %>%
  tally() %>%
  ungroup()

# AUC in single units
auc_to_plot = mutate(
  plot_data,
  auc = as.numeric(auc),
  auc_cut = cut(auc, breaks = seq(0, 1, 0.01)),
  num = as.numeric(auc_cut),
  doi = factor(doi)
) %>%
  group_by(num) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(doi, auc, auc_cut, num, n) %>%
  filter(num >= 49) %>%
  na.omit()

# check how many unique articles are at each bin
auc_to_plot %>%
  group_by(num) %>%
  mutate(n = n_distinct(doi)) %>%
  distinct(num, .keep_all = TRUE) %>%
  ggplot(aes(x = num, y = n)) +
  geom_col()


# code from Adrian here, see 10_analysis_adrian.R
# counts for every bin for every paper
bin_counts = mutate(
  plot_data,
  auc = as.numeric(auc),
  auc_cut = cut(auc, breaks = seq(0, 1, 0.01)),
  num = as.numeric(auc_cut),
  numf = factor(num, levels = 9:100)
) %>%
  filter(!is.na(num)) %>% # remove one AUC over 1
  group_by(doi) %>%
  count(numf, .drop = FALSE) %>% # include zeros; using factor
  ungroup()

# check of balanced data:
table(table(bin_counts$doi))

# for model, just model upper half of AUC
for_model = mutate(
  bin_counts,
  doi = as.numeric(as.factor(doi)), # for random effect below
  num = as.numeric(as.character(numf))
) %>%
  filter(num >= 50)

# loop through degrees of freedom for p-spline
preds = AIC = NULL
for (b.knots in 0:1) {
  # varying boundary knots to check sensitivity to knot location
  boundary.knots = c(50, 100) # knots on edge of data
  if (b.knots == 1) {
    boundary.knots = c(40, 110)
  } # ; knots at a distance from edge
  for (df in 2:4) {
    # add spline basis as data
    bs_basis = bs(
      for_model$num,
      Boundary.knots = boundary.knots,
      k = df,
      degree = 3,
      intercept = FALSE
    ) # b-spline
    #ps_basis = splines::ps(for_model$num, k = df)
    bs = data.frame(bs_basis[1:nrow(for_model), ])
    names(bs) = paste('s', 1:df, sep = '')
    for_this_model = bind_cols(for_model, bs)
    # make formula dependent on degrees of freedom; with random effect for paper
    formula = paste(
      'n ~ ',
      paste('s', 1:df, collapse = ' + ', sep = ''),
      "+ s(doi, bs = 're')"
    )
    #
    fit_ri <- bam(
      as.formula(formula),
      family = nb(), # negative binomial
      #  family = poisson(), # Poisson
      data = for_this_model,
      method = "fREML"
    )
    # AIC
    this_aic = data.frame(AIC = AIC(fit_ri), df = df, b.knots = b.knots)
    AIC = bind_rows(AIC, this_aic)
    
    ## predictions
    # Build a prediction grid (needed below)
    pred_grid <- data.frame(
      num = seq(50, 100, length.out = 200),
      doi = 1,
      df = df,
      b.knots = b.knots
    )
    bs_pred = bs(
      pred_grid$num,
      degree = attr(bs_basis, 'degree'), # use knots/degree from basis above
      knots = attr(bs_basis, 'knots'),
      Boundary.knots = attr(bs_basis, "Boundary.knots")
    )
    bs_pred = data.frame(bs_pred[1:nrow(pred_grid), ])
    names(bs_pred) = paste('s', 1:df, sep = '')
    pred_grid = bind_cols(pred_grid, bs_pred)
    pred_grid$fitted <- predict(
      fit_ri,
      newdata = pred_grid,
      type = "response",
      re.form = ~0 # no random effect
    )
    # concatenate
    preds = bind_rows(preds, pred_grid)
  } # end of loops
}

# 4 degrees of freedom is no different as model is not converging; a sign that there are too many knots
for_plot = filter(preds)
AIC = filter(AIC)

# plot the splines
cols = c('brown3', 'darkorchid2', 'darkseagreen3')

pplot = ggplot(
  data = for_plot,
  aes(x = num / 100, y = fitted, linetype = factor(b.knots), col = factor(df))
) +
  geom_line(size = 1.05) +
  xlab('AUC') +
  scale_color_manual('Degrees\nof freedom', values = cols) +
  scale_linetype_manual(
    'Boundary\nknots',
    values = 1:2,
    labels = c('Edge', 'Distance')
  ) +
  theme_bw()

pplot

# AIC table from best to worst; boundary knots make no difference (gave same fit)
round(arrange(AIC, AIC), digits = 1)
# refit the best model
best = filter(preds, b.knots == 0, df == 3) # best model

fit_best <- bam(
  n ~ ns(num, df = 3) + s(doi, bs = 're'),
  family = nb(), # negative binomial
  data = for_model,
  method = "fREML"
)

## residuals
total_AUC = sum(for_model$n)

auc_plot_data = group_by(for_model, num) %>%
  summarise(n = sum(n)) %>% # get the observed counts
  ungroup() %>%
  mutate(doi = 1) %>% # do not care about paper for this plot
  mutate(
    fitted = predict(
      fit_best,
      newdata = .,
      type = "response",
      exclude = "s(doi)"
    )
  ) %>%
  mutate(
    fitted = fitted / sum(fitted), # normalise density
    fitted = fitted * total_AUC, # scale by total number of AUCs
    resid_raw = n - fitted
  )

# check that residuals sum to zero
sum(auc_plot_data$resid_raw)


# Build a prediction grid
pred_grid <- data.frame(num = seq(50, 100, length.out = 200),
                        doi = 1)

# glmer: population-level (random intercept set to 0)
pred_grid$fitted <- predict(
  fit_best,
  newdata = pred_grid,
  type = "response",
  re.form = NA
)


# add labels to the plot

# make the auc distribution plot
auc_plot <- ggplot(data = auc_to_plot2, aes(x = num, y = n)) +
  geom_bar(
    aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
    stat = 'identity',
    width = 1,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#B52B12", "FALSE" = "#FFBDAD"),
    guide = "none"
  ) +
  geom_line(
    data = auc_plot_data,
    aes(x = num, y = fitted),
    linewidth = 1,
    linetype = "dashed"
  ) +
  xlab('AUC Value') +
  ylab('Frequency') +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  ) +
  #scale_y_continuous(limits = c(0, 550)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# check the plot
auc_plot

# save the plot
ggsave(
  plot = auc_plot,
  width = 12,
  height = 10,
  dpi = 500,
  filename = "03_figures/auc_distribution_2.jpg"
)


# Bar plot of residuals
resid_plot <- ggplot(auc_plot_data, aes(x = num, y = resid_raw)) +
  geom_bar(
    aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
    stat = "identity",
    width = 1,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#B52B12", "FALSE" = "#FFBDAD"),
    guide = "none"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  xlab("AUC Value") +
  ylab("Residual (observed - expected)") +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  ) +
  scale_y_continuous(limits = c(-50, 100)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
resid_plot

# save the plot
ggsave(
  plot = resid_plot,
  width = 12,
  height = 10,
  dpi = 500,
  filename = "03_figures/auc_residuals_2.jpg"
)


# combine the plots for values into three seperate plots
auc_final <- (auc_plot + labs(tag = "A")) / (resid_plot + labs(tag = "B"))


ggsave(
  auc_final,
  file = "03_figures/auc_final_2.jpg",
  width = 8,
  height = 10,
  dpi = 500
)




# get the mean sensitivity and specificity values
mean(na.omit(plot_data$sens))
mean(na.omit(plot_data$spec))

sd(na.omit(plot_data$sens))
sd(na.omit(plot_data$spec))


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


fifty_ss <- cumulative_data$sample_size[which.min(abs(
  cumulative_data$cumulative_pct - 50
))]
ninty_ss <- cumulative_data$sample_size[which.min(abs(
  cumulative_data$cumulative_pct - 90
))]

cumulative_sample_size <- cumulative_data %>%
  ggplot(aes(x = log(sample_size), y = cumulative_pct)) +
  geom_line() +
  labs(x = "Sample Size (Log Scale)", y = "Proportion of Models") +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels = c(
      "0",
      "0.1",
      "0.2",
      "0.3",
      "0.4",
      "0.5",
      "0.6",
      "0.7",
      "0.8",
      "0.9",
      "1"
    )
  ) +
  annotate(
    "segment",
    x = log(fifty_ss),
    xend = log(fifty_ss),
    y = 0,
    yend = 50,
    linetype = "dashed",
    color = "black"
  ) +
  annotate(
    "segment",
    x = min(log(cumulative_data$sample_size)),
    xend = log(fifty_ss),
    y = 50,
    yend = 50,
    linetype = "dashed",
    color = "black"
  ) +
  annotate(
    "segment",
    x = log(ninty_ss),
    xend = log(ninty_ss),
    y = 0,
    yend = 90,
    linetype = "dashed",
    color = "black"
  ) +
  annotate(
    "segment",
    x = min(log(cumulative_data$sample_size)),
    xend = log(ninty_ss),
    y = 90,
    yend = 90,
    linetype = "dashed",
    color = "black"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggsave(
  cumulative_sample_size,
  file = "03_figures/cumulative_sample_size.jpg",
  width = 6,
  height = 4,
  dpi = 500
)

# gather the summary data of the articles and models

min(na.omit(plot_data$sample_size))
max(na.omit(plot_data$sample_size))
median(na.omit(plot_data$sample_size))
Mode(na.omit(plot_data$sample_size))

# see if there were any AUC reported below 0 or above 1
plot_data %>% filter(auc > 1)
plot_data %>% filter(auc < 0)

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


# get summary data for the AUC value
plot_data %>%
  filter(auc > 0) %>%
  group_by(doi) %>%
  summarise(n = n()) %>%
  mutate(
    mean = mean(n),
    median = median(n),
    iqr_25 = quantile(n, probs = 0.25),
    iqr_75 = quantile(n, probs = 0.75)
  )


# plot sample size against AUC
# create dataframe for the plot
auc_sample_plot_data = plot_data %>%
  mutate(
    auc = as.numeric(auc),
    auc_cut = cut(auc, breaks = seq(0, 1, 0.01)),
    num = as.numeric(auc_cut)
  ) %>%
  group_by(auc_cut, num) %>%
  summarise(
    sample = median(sample_size, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# plot the smaple size to auc bins
auc_sample_plot <- auc_sample_plot_data %>%
  ggplot(aes(x = num, y = sample)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  ) +
  scale_y_continuous(limits = c(0, 500)) +
  labs(x = "AUC Value", y = "Median Sample Size") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


auc_sample_plot_2 <- auc_sample_plot_data %>%
  ggplot(aes(x = num, y = sample)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    data = auc_sample_plot_data %>% filter(num < 71),
    colour = "red"
  ) +
  geom_smooth(
    method = "lm",
    data = auc_sample_plot_data %>% filter(num > 69),
    colour = "red"
  ) +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  ) +
  scale_y_continuous(limits = c(0, 500)) +
  labs(x = "AUC Value", y = "Median Sample Size") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


auc_sample_plot
auc_sample_plot_2

auc_sample_plots <- (auc_sample_plot + labs(tag = "A")) /
  (auc_sample_plot_2 + labs(tag = "B"))

# save the plot
ggsave(
  auc_sample_plots,
  file = "03_figures/auc_sample_plot.jpg",
  width = 8,
  height = 12,
  dpi = 500
)
