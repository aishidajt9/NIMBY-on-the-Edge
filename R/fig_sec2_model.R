# =============================================================================
# Figure Generation for Section 2.3 (Numerical Examples)
# =============================================================================
#
# Paper: NIMBY on the Edge: A One-dimensional Model of NIMBY Facility Siting
# Authors: Atsushi Ishida (Kwansei Gakuin University)
#          Yutaka Maeda (Shinshu University)
# Journal: Journal of Mathematical Sociology
#
# Generates Figures 1-5: utility functions, population distributions,
# and social welfare under three utility specifications.
#
# Requirements:
#   - R >= 4.1.0
#   - tidyverse >= 2.0.0
#   - statmod >= 1.5.0
#
# Usage:
#   source("fig_numerical_examples.R")
#   generate_all_figures()
#
# License: MIT
# =============================================================================

library(tidyverse)
library(statmod)

# =============================================================================
# Parameters
# =============================================================================
B0 <- 10
C0 <- 15

# =============================================================================
# Plot Theme
# =============================================================================
theme_paper <- theme_minimal(base_size = 11, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.4),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )

# =============================================================================
# Color Palettes (ColorBrewer Dark2)
# =============================================================================
colors_utility <- c(
  "Linear" = "#1b9e77",
  "Exp (1.0)" = "#d95f02",
  "Exp (1.8)" = "#7570b3"
)

colors_dist <- c(
  "Beta(1,1)" = "#1b9e77",
  "Beta(2,2)" = "#d95f02",
  "Beta(1,3)" = "#7570b3",
  "Beta(0.2,0.2)" = "#e7298a"
)

# =============================================================================
# Utility Functions
# =============================================================================
u_linear <- function(x, x_N, B0 = 10, C0 = 15, gamma = 10) {
  d <- abs(x - x_N)
  B0 - C0 + gamma * d
}

u_exp_weak <- function(x, x_N, B0 = 10, C0 = 15, delta = 1.0) {
  d <- abs(x - x_N)
  B0 - C0 * exp(-delta * d)
}

u_exp_strong <- function(x, x_N, B0 = 10, C0 = 15, delta = 1.8) {
  d <- abs(x - x_N)
  B0 - C0 * exp(-delta * d)
}

# =============================================================================
# Welfare Computation
# =============================================================================
compute_welfare <- function(x_N, u_func, dist_alpha, dist_beta, n_nodes = 100) {
  gq <- gauss.quad.prob(n_nodes, dist = "beta", alpha = dist_alpha, beta = dist_beta)
  sum(gq$weights * u_func(gq$nodes, x_N))
}

find_optimal <- function(W_values, x_N_grid, tol = 1e-6) {
  max_W <- max(W_values)
  optimal_indices <- which(W_values >= max_W - tol)
  optimal_x_N <- x_N_grid[optimal_indices]
  optimal_W <- W_values[optimal_indices]
  a <- min(x_N_grid)
  b <- max(x_N_grid)
  is_boundary <- all(optimal_x_N <= a + 0.01 | optimal_x_N >= b - 0.01)
  list(x_N = optimal_x_N, W = optimal_W, max_W = max_W, is_boundary = is_boundary)
}

# =============================================================================
# Distribution and Utility Definitions
# =============================================================================
distributions <- list(
  list(name = "Beta(1,1)", alpha = 1, beta = 1),
  list(name = "Beta(2,2)", alpha = 2, beta = 2),
  list(name = "Beta(1,3)", alpha = 1, beta = 3),
  list(name = "Beta(0.2,0.2)", alpha = 0.2, beta = 0.2)
)

utility_funcs <- list(
  list(name = "Linear", func = u_linear),
  list(name = "Exp (1.0)", func = u_exp_weak),
  list(name = "Exp (1.8)", func = u_exp_strong)
)

dist_levels <- c("Beta(1,1)", "Beta(2,2)", "Beta(1,3)", "Beta(0.2,0.2)")
utility_levels <- c("Linear", "Exp (1.0)", "Exp (1.8)")

# =============================================================================
# Figure 1: Utility Functions
# =============================================================================
fig1_utility <- function() {
  x_grid <- seq(0, 1, length.out = 100)
  x_N_fixed <- 0.5

  df <- tibble(
    x = rep(x_grid, 3),
    utility = c(
      map_dbl(x_grid, ~u_linear(.x, x_N_fixed)),
      map_dbl(x_grid, ~u_exp_weak(.x, x_N_fixed)),
      map_dbl(x_grid, ~u_exp_strong(.x, x_N_fixed))
    ),
    case = factor(rep(utility_levels, each = length(x_grid)), levels = utility_levels)
  )

  ggplot(df, aes(x = x, y = utility, color = case)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = colors_utility, name = NULL) +
    labs(x = expression(x ~ "(resident location)"),
         y = expression(u("|" * x - x[N] * "|") ~ "(utility)")) +
    theme_paper
}

# =============================================================================
# Figure 2: Population Distributions
# =============================================================================
fig2_distributions <- function() {
  x_seq <- seq(0.001, 0.999, length.out = 200)

  df <- tibble(
    x = rep(x_seq, 4),
    density = c(
      dbeta(x_seq, 1, 1),
      dbeta(x_seq, 2, 2),
      dbeta(x_seq, 1, 3),
      dbeta(x_seq, 0.2, 0.2)
    ),
    distribution = factor(rep(dist_levels, each = length(x_seq)), levels = dist_levels)
  )

  ggplot(df, aes(x = x, y = density, color = distribution)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = colors_dist, name = NULL) +
    coord_cartesian(ylim = c(0, 5)) +
    labs(x = expression(x ~ "(location)"),
         y = expression(f(x) ~ "(density)")) +
    theme_paper
}

# =============================================================================
# Welfare Figure Helper Function
# =============================================================================
fig_welfare <- function(utility_idx) {
  x_N_grid <- seq(0, 1, length.out = 101)
  u_func <- utility_funcs[[utility_idx]]$func

  # Compute welfare for all distributions
  results <- map(distributions, function(dist) {
    W_values <- map_dbl(x_N_grid, ~compute_welfare(.x, u_func, dist$alpha, dist$beta))
    tibble(x_N = x_N_grid, W = W_values, distribution = dist$name)
  }) %>%
    list_rbind() %>%
    mutate(distribution = factor(distribution, levels = dist_levels))

  # Find optimal points
  opt_points <- results %>%
    group_by(distribution) %>%
    group_modify(~ {
      opt <- find_optimal(.x$W, .x$x_N)
      tibble(x_N = opt$x_N, W = opt$W)
    }) %>%
    ungroup()

  ggplot(results, aes(x = x_N, y = W)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(data = opt_points, color = "red", size = 2.5) +
    facet_wrap(~ distribution, ncol = 2, scales = "free_y") +
    labs(x = expression(x[N] ~ "(facility location)"),
         y = expression(W(x[N]) ~ "(social welfare)")) +
    theme_paper
}

# =============================================================================
# Figure 3-5: Welfare under different utility functions
# =============================================================================
fig3_welfare_linear <- function() fig_welfare(1)
fig4_welfare_exp_weak <- function() fig_welfare(2)
fig5_welfare_exp_strong <- function() fig_welfare(3)

# =============================================================================
# Generate and Save All Figures
# =============================================================================
generate_all_figures <- function(output_dir = "figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Figure 1: Utility functions
  p1 <- fig1_utility()
  ggsave(file.path(output_dir, "fig1_utility_functions.pdf"), p1,
         width = 6, height = 3.5, dpi = 600)
  cat("Saved: fig1_utility_functions.pdf\n")

  # Figure 2: Distributions
  p2 <- fig2_distributions()
  ggsave(file.path(output_dir, "fig2_distributions.pdf"), p2,
         width = 6, height = 3.5, dpi = 600)
  cat("Saved: fig2_distributions.pdf\n")

  # Figure 3: Linear welfare
  p3 <- fig3_welfare_linear()
  ggsave(file.path(output_dir, "fig3_welfare_linear.pdf"), p3,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig3_welfare_linear.pdf\n")

  # Figure 4: Exp weak welfare
  p4 <- fig4_welfare_exp_weak()
  ggsave(file.path(output_dir, "fig4_welfare_exp_weak.pdf"), p4,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig4_welfare_exp_weak.pdf\n")

  # Figure 5: Exp strong welfare
  p5 <- fig5_welfare_exp_strong()
  ggsave(file.path(output_dir, "fig5_welfare_exp_strong.pdf"), p5,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig5_welfare_exp_strong.pdf\n")

  cat("\nAll figures generated successfully.\n")
}

# =============================================================================
# Display Figures (for interactive use)
# =============================================================================
# Uncomment to display:
# print(fig1_utility())
# print(fig2_distributions())
# print(fig3_welfare_linear())
# print(fig4_welfare_exp_weak())
# print(fig5_welfare_exp_strong())

# =============================================================================
# Save All Figures
# =============================================================================
# Uncomment to generate:
# generate_all_figures()
