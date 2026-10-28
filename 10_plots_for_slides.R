# 10_plots_for_slides.R
# code from Adrian to make nice plots for a slide
# uses 10_analysis.R

# a) bar plot
auc_plot = auc_plot +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    expand = 0
  ) +
  scale_y_continuous(expand = 0, limits = c(0, 520))

# save the plot
ggsave(
  plot = auc_plot,
  width = 14,
  height = 10,
  units = 'cm',
  dpi = 500,
  filename = "03_figures/auc_distribution_for_slide.jpg"
)

# b) sample size plot
auc_sample_plot = auc_sample_plot +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    expand = 0
  ) +
  scale_y_continuous(expand = 0, limits = c(0, 520))

# save the plot
ggsave(
  plot = auc_sample_plot,
  width = 14,
  height = 10,
  units = 'cm',
  dpi = 500,
  filename = "03_figures/auc_sample_plot_for_slide.jpg"
)
