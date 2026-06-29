# =============================================================================
# Figure Generation for Section 3 (Incorporating Siting Feasibility)
# =============================================================================
#
# Paper: NIMBY on the Edge: A One-dimensional Model of NIMBY Facility Siting
# Authors: Atsushi Ishida (Kwansei Gakuin University)
#          Yutaka Maeda (Shinshu University)
# Journal: Journal of Mathematical Sociology
#
# Generates Figures 6-9: optimal location trajectories as alpha varies,
# and composite objective function shapes.
#
# Requirements:
#   - R >= 4.1.0
#   - tidyverse >= 2.0.0
#   - statmod >= 1.5.0
#   - patchwork >= 1.1.0
#
# Usage:
#   source("fig_siting_feasibility.R")
#   generate_section3_figures()
#
# License: MIT
# =============================================================================

library(tidyverse)
library(statmod)
library(patchwork)

# =============================================================================
# Parameters
# =============================================================================
B0 <- 10
C0 <- 15

# =============================================================================
# Plot Theme (consistent with Section 2 figures)
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
# Utility Functions (same as Section 2)
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
# Core Computation Functions
# =============================================================================

# Social welfare W(x_N) via Gauss quadrature
compute_welfare <- function(x_N, u_func, dist_alpha, dist_beta, n_nodes = 100) {
  gq <- gauss.quad.prob(n_nodes, dist = "beta", alpha = dist_alpha, beta = dist_beta)
  sum(gq$weights * u_func(gq$nodes, x_N))
}

# Normalized welfare W_tilde (degenerate case: return 0)
normalize_welfare <- function(W_values) {
  W_min <- min(W_values)
  W_max <- max(W_values)
  if (W_max - W_min < 1e-10) return(rep(0, length(W_values)))
  (W_values - W_min) / (W_max - W_min)
}

# Normalized siting feasibility S_tilde (degenerate case: return 0)
normalize_siting_feasibility <- function(x_N_grid, dist_alpha, dist_beta) {
  f_values <- dbeta(x_N_grid, dist_alpha, dist_beta)
  f_max <- max(f_values)
  f_min <- min(f_values)
  if (f_max - f_min < 1e-10) return(rep(0, length(x_N_grid)))
  (f_max - f_values) / (f_max - f_min)
}

# Find optimal locations (handle multiple optima)
find_optima <- function(W_tilde, S_tilde, x_N_grid, alpha, tol = 1e-6) {
  phi <- alpha * W_tilde + (1 - alpha) * S_tilde
  max_phi <- max(phi)
  optimal_indices <- which(phi >= max_phi - tol)
  x_N_grid[optimal_indices]
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
# Grid Settings
# =============================================================================
x_N_grid <- seq(0.01, 0.99, length.out = 99)
alpha_grid <- seq(0, 1, length.out = 101)

# =============================================================================
# Compute Normalized Functions for All 12 Cases
# =============================================================================
compute_normalized_data <- function() {
  results <- list()

  for (dist in distributions) {
    for (util in utility_funcs) {
      W_values <- map_dbl(x_N_grid, ~compute_welfare(.x, util$func, dist$alpha, dist$beta))
      W_tilde <- normalize_welfare(W_values)
      S_tilde <- normalize_siting_feasibility(x_N_grid, dist$alpha, dist$beta)

      results[[length(results) + 1]] <- tibble(
        x_N = x_N_grid,
        W_tilde = W_tilde,
        S_tilde = S_tilde,
        dist_name = dist$name,
        utility_type = util$name
      )
    }
  }

  bind_rows(results) %>%
    mutate(
      dist_name = factor(dist_name, levels = dist_levels),
      utility_type = factor(utility_type, levels = utility_levels)
    )
}

# =============================================================================
# Compute Optimal Location Trajectories
# =============================================================================
compute_trajectory_data <- function(normalized_data) {
  normalized_data %>%
    group_by(dist_name, utility_type) %>%
    group_modify(~ {
      W_tilde_vec <- .x$W_tilde
      S_tilde_vec <- .x$S_tilde
      x_N_vec <- .x$x_N

      map(alpha_grid, function(a) {
        optima <- find_optima(W_tilde_vec, S_tilde_vec, x_N_vec, a)
        tibble(alpha = a, x_N_star = optima)
      }) %>%
        list_rbind()
    }) %>%
    ungroup()
}

# =============================================================================
# Figure 6: Trajectory - Linear Utility
# =============================================================================
fig6_trajectory_linear <- function(trajectory_data) {
  trajectory_data %>%
    filter(utility_type == "Linear") %>%
    ggplot(aes(x = alpha, y = x_N_star)) +
    geom_point(size = 0.8, alpha = 0.7, color = "steelblue") +
    facet_wrap(~dist_name, ncol = 2) +
    labs(
      x = expression(alpha ~ "(weight on social welfare)"),
      y = expression(x[N]^"*" ~ "(optimal location)")
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_paper
}

# =============================================================================
# Figure 7: Trajectory - Exp (1.0) Utility
# =============================================================================
fig7_trajectory_exp_weak <- function(trajectory_data) {
  trajectory_data %>%
    filter(utility_type == "Exp (1.0)") %>%
    ggplot(aes(x = alpha, y = x_N_star)) +
    geom_point(size = 0.8, alpha = 0.7, color = "steelblue") +
    facet_wrap(~dist_name, ncol = 2) +
    labs(
      x = expression(alpha ~ "(weight on social welfare)"),
      y = expression(x[N]^"*" ~ "(optimal location)")
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_paper
}

# =============================================================================
# Figure 8: Trajectory - Exp (1.8) Utility
# =============================================================================
fig8_trajectory_exp_strong <- function(trajectory_data) {
  trajectory_data %>%
    filter(utility_type == "Exp (1.8)") %>%
    ggplot(aes(x = alpha, y = x_N_star)) +
    geom_point(size = 0.8, alpha = 0.7, color = "steelblue") +
    facet_wrap(~dist_name, ncol = 2) +
    labs(
      x = expression(alpha ~ "(weight on social welfare)"),
      y = expression(x[N]^"*" ~ "(optimal location)")
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_paper
}

# =============================================================================
# Figure 9: Composite Objective Function Shape (faceted 2-panel figure)
# =============================================================================
fig9_phi_shape <- function(normalized_data) {
  alpha_values <- c(0, 0.5, 1)

  # Combine two cases into one data frame with panel labels
  df <- bind_rows(
    normalized_data %>%
      filter(dist_name == "Beta(2,2)", utility_type == "Exp (1.0)") %>%
      mutate(panel = "(a) Beta(2,2), Exp (1.0)"),
    normalized_data %>%
      filter(dist_name == "Beta(0.2,0.2)", utility_type == "Linear") %>%
      mutate(panel = "(b) Beta(0.2,0.2), Linear")
  ) %>%
    expand_grid(alpha = alpha_values) %>%
    mutate(
      phi = alpha * W_tilde + (1 - alpha) * S_tilde,
      alpha_label = factor(paste0("alpha = ", alpha), levels = paste0("alpha = ", alpha_values)),
      panel = factor(panel, levels = c("(a) Beta(2,2), Exp (1.0)", "(b) Beta(0.2,0.2), Linear"))
    )

  ggplot(df, aes(x = x_N, y = phi, color = alpha_label)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(~panel, ncol = 2) +
    labs(
      x = expression(x[N] ~ "(facility location)"),
      y = expression(phi(x[N]) ~ "(composite objective)"),
      color = NULL
    ) +
    theme_paper
}

# =============================================================================
# Generate and Save All Section 3 Figures
# =============================================================================
generate_section3_figures <- function(output_dir = "figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("Computing normalized data...\n")
  normalized_data <- compute_normalized_data()

  cat("Computing trajectory data...\n")
  trajectory_data <- compute_trajectory_data(normalized_data)

  # Figure 6: Linear trajectory
  cat("Generating Figure 6...\n")
  p6 <- fig6_trajectory_linear(trajectory_data)
  ggsave(file.path(output_dir, "fig6_trajectory_linear.pdf"), p6,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig6_trajectory_linear.pdf\n")

  # Figure 7: Exp weak trajectory
  cat("Generating Figure 7...\n")
  p7 <- fig7_trajectory_exp_weak(trajectory_data)
  ggsave(file.path(output_dir, "fig7_trajectory_exp_weak.pdf"), p7,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig7_trajectory_exp_weak.pdf\n")

  # Figure 8: Exp strong trajectory
  cat("Generating Figure 8...\n")
  p8 <- fig8_trajectory_exp_strong(trajectory_data)
  ggsave(file.path(output_dir, "fig8_trajectory_exp_strong.pdf"), p8,
         width = 7, height = 5, dpi = 600)
  cat("Saved: fig8_trajectory_exp_strong.pdf\n")

  # Figure 9: Phi shape (combined 2-panel figure)
  # 2 panels side-by-side + right legend: width 8, height 3.5
  # NOTE: For SSRN final version, add width=\textwidth to \includegraphics in TeX
  cat("Generating Figure 9...\n")
  p9 <- fig9_phi_shape(normalized_data)
  ggsave(file.path(output_dir, "fig9_phi_shape.pdf"), p9,
         width = 8, height = 3.5, dpi = 600)
  cat("Saved: fig9_phi_shape.pdf\n")

  cat("\nAll Section 3 figures generated successfully.\n")

  # Return data for inspection
  invisible(list(normalized_data = normalized_data, trajectory_data = trajectory_data))
}

# =============================================================================
# Display Figures (for interactive use)
# =============================================================================
# Uncomment to display:
# normalized_data <- compute_normalized_data()
# trajectory_data <- compute_trajectory_data(normalized_data)
# print(fig6_trajectory_linear(trajectory_data))
# print(fig7_trajectory_exp_weak(trajectory_data))
# print(fig8_trajectory_exp_strong(trajectory_data))
# p9 <- fig9_phi_shape(normalized_data)
# print(p9)

# =============================================================================
# Generate All Figures
# =============================================================================
# Uncomment to generate:
# generate_section3_figures()
