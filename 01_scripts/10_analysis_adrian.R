# 10_analysis_adrian.R
# Oct 2026

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
for_plot = filter(preds, df < 4)
AIC = filter(AIC, df < 4)

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
arrange(AIC, AIC)
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

# your code should work from here ...









