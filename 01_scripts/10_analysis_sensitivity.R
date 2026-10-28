# 10_analysis_sensitivity.R

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


# now for the data that is here plot the sensitivity in a distribution
# sens in single units
sens_to_plot = mutate(
  plot_data,
  sens = as.numeric(sens),
  sens_cut = cut(sens, breaks = seq(0, 100, 1)),
  num = as.numeric(sens_cut),
  doi = factor(doi)
) %>%
  group_by(num) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(doi, sens, sens_cut, num, n) %>%
  filter(num > 0) %>%
  na.omit()

# Sens in single units
sens_to_plot2 = mutate(
  plot_data,
  sens = as.numeric(sens),
  sens_cut = cut(sens, breaks = seq(0, 100, 1)),
  num = as.numeric(sens_cut)
) %>%
  group_by(sens_cut, num) %>%
  tally() %>%
  ungroup()

# counts for every bin for every paper
bin_counts = mutate(
  plot_data,
  sens = as.numeric(sens),
  sens_cut = cut(sens, breaks = seq(0, 100, 1)),
  num = as.numeric(sens_cut),
  numf = factor(num, levels = 1:100)
) %>%
  filter(!is.na(num)) %>% # remove one sens over 1
  group_by(doi) %>%
  count(numf, .drop = FALSE) %>% # include zeros; using factor
  ungroup()

# check of balanced data:
table(table(bin_counts$doi))

# for model, just model upper half of sens
for_model = mutate(
  bin_counts,
  doi = as.numeric(as.factor(doi)), # for random effect below
  num = as.numeric(as.character(numf))
)

# loop through degrees of freedom for p-spline
preds = AIC = NULL
for (b.knots in 0:1) {
  # varying boundary knots to check sensitivity to knot location
  boundary.knots = c(0, 100) # knots on edge of data
  if (b.knots == 1) {
    boundary.knots = c(-10, 110)
  } # ; knots at a distance from edge
  for (df in 2:6) {
    # add spline basis as data
    bs_basis = bs(
      for_model$num,
      Boundary.knots = boundary.knots,
      k = df,
      degree = 5,
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
      num = seq(0, 100, length.out = 200),
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
cols = c('brown3', 'darkorchid2', 'darkseagreen3', 'red', 'blue')

pplot = ggplot(
  data = for_plot,
  aes(x = num / 100, y = fitted, linetype = factor(b.knots), col = factor(df))
) +
  geom_line(size = 1.05) +
  xlab('sens') +
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
best = filter(preds, b.knots == 1, df == 4) # best model

fit_best <- bam(
  n ~ ns(num, df = 4) + s(doi, bs = 're'),
  family = nb(), # negative binomial
  data = for_model,
  method = "fREML"
)

## residuals
total_sens = sum(for_model$n)

sens_plot_data = group_by(for_model, num) %>%
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
    fitted = fitted * total_sens, # scale by total number of senss
    resid_raw = n - fitted
  )

# check that residuals sum to zero
sum(sens_plot_data$resid_raw)



# Build a prediction grid
pred_grid <- data.frame(num = seq(0, 100, length.out = 200),
                        doi = 1)

# glmer: population-level (random intercept set to 0)
pred_grid$fitted <- predict(
  fit_best,
  newdata = pred_grid,
  type = "response",
  re.form = NA
)



sens_plot <- ggplot(data = sens_to_plot2, aes(x = num, y = n)) +
  geom_bar(
    aes(fill = num %in% c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
    stat = 'identity',
    width = 1,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#346E29", "FALSE" = "#AED9A7"),
    guide = "none"
  ) +
  geom_line(
    data = sens_plot_data,
    aes(x = num, y = fitted),
    linewidth = 1,
    linetype = "dashed"
  ) +
  xlab('Sensitivity Value (%)') +
  ylab('Frequency') +
  scale_x_continuous(
    limits = c(0, 101),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  ) +
  scale_y_continuous(limits = c(0, 325)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# check the plot
sens_plot


# save the plot
ggsave(
  plot = sens_plot,
  width = 12,
  height = 10,
  dpi = 500,
  filename = "03_figures/sens_distribution_2.jpg"
)



# Bar plot of residuals
sens_resid_plot <- ggplot(sens_plot_data, aes(x = num, y = resid_raw)) +
  geom_bar(
    aes(fill = num %in% c(50, 60, 70, 80, 90, 100)),
    stat = "identity",
    width = 1,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#346E29", "FALSE" = "#AED9A7"),
    guide = "none"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  xlab("Sensitivity Value (%)") +
  ylab("Residual (observed - expected)") +
  scale_x_continuous(
    limits = c(0, 101),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  ) +
  scale_y_continuous(limits = c(-100, 200)) +
  # geom_text(data = label2, aes(x = num, y = n, label = text), size = 6.5, adj = 0, col = label_colour)+
  # geom_segment(aes(x = 38, y = 135, xend = 50, yend = 36), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  # geom_segment(aes(x = 38, y = 135, xend = 59, yend = 34), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  # geom_segment(aes(x = 38, y = 135, xend = 68, yend = 36), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  # geom_segment(aes(x = 38, y = 135, xend = 78, yend = 42), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  # geom_segment(aes(x = 38, y = 135, xend = 98, yend = 123), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# check the plot
sens_resid_plot

# save the plot
ggsave(
  plot = sens_resid_plot,
  width = 12,
  height = 10,
  dpi = 500,
  filename = "03_figures/sens_residuals_2.jpg"
)

sens_final <- (sens_plot + labs(tag = "A")) /
  (sens_resid_plot + labs(tag = "B"))

ggsave(
  sens_final,
  file = "03_figures/sens_final_2.jpg",
  width = 8,
  height = 10,
  dpi = 500
)
