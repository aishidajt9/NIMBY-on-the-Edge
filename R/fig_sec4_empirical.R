# =============================================================================
# Figure Generation for Section 4 (Empirical Analysis)
# =============================================================================
#
# Paper: NIMBY on the Edge: A One-dimensional Model of NIMBY Facility Siting
# Authors: Atsushi Ishida (Kwansei Gakuin University)
#          Yutaka Maeda (Shinshu University)
# Journal: Journal of Mathematical Sociology
#
# Generates Figures 10-13:
#   - Fig 10: Tokyo map with MSW facility locations
#   - Fig 11: Osaka map with MSW facility locations
#   - Fig 12: Distance distribution histogram with ECDF
#   - Fig 13: Distance vs Relative Density scatterplot
#
# Data:
#   - DATA.rds: Facility-level data (publicly available)
#   - Shapefiles for maps: Download from e-Stat and MLIT (see comments below)
#
# DATA.rds structure:
#   - ID: Facility ID
#   - Year: Year of operation start (2000-2012)
#   - Municipality: Municipal code (5-digit JIS code, stored as numeric)
#   - Distance: Distance to municipal boundary (meters)
#   - Cross: Inter-municipal facility flag
#   - RelDens: Relative density (facility block density / municipal average density)
#   - N: Number of census blocks in the municipality
#   - geometry: Facility location (sf point, WGS84)
#
# DATA.rds generation code available upon reasonable request.
#
# Requirements:
#   - R >= 4.1.0
#   - tidyverse >= 2.0.0
#   - sf >= 1.0.0
#   - ggspatial >= 1.1.0
#
# Usage:
#   source("fig_sec4_empirical.R")
#   generate_section4_figures()
#
# License: MIT
# =============================================================================

library(tidyverse)
library(sf)
library(ggspatial)

# =============================================================================
# Plot Theme (consistent with Section 2-3 figures)
# =============================================================================
theme_paper <- theme_minimal(base_size = 11, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.4),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )

# Map theme (simpler, no axis lines)
theme_map <- theme_minimal(base_size = 11, base_family = "Helvetica") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# =============================================================================
# Color Palette for Maps
# =============================================================================
map_colors <- c("#E6F3FF", "#7FB3D5", "#45B39D", "#28B463", "#9ECD2C", "#F1C40F")

# =============================================================================
# Constants
# =============================================================================
# Tokyo island area codes (excluded from analysis)
TOKYO_ISLANDS <- c("13361", "13362", "13363", "13364",
                   "13381", "13382", "13401", "13402", "13421")
TOKYO_ISLANDS_NUM <- as.numeric(TOKYO_ISLANDS)

# =============================================================================
# Data Loading
# =============================================================================

# Main facility data (publicly available)
load_data <- function(data_path = "R/DATA.rds") {
  read_rds(data_path)
}

# Tokyo census data (2000)
# Download from: https://www.e-stat.go.jp/
# File: Census 2000, Tokyo (code 13), block-level (sho-chiiki)
# NOTE: Island areas are excluded (see TOKYO_ISLANDS constant)
load_tokyo_census <- function(path = "spatial_R/CENSUS2000/13/h12ka13.shp") {
  read_sf(path) %>%
    filter(!paste0(PREF, CITY) %in% TOKYO_ISLANDS) %>%
    mutate(`Population Density` = JINKO / (AREA / 1000000))
}

# Tokyo municipal boundary
# Download from: https://nlftp.mlit.go.jp/
load_tokyo_boundary <- function(path = "spatial_R/area_municipality/N03-001001_13-g_AdministrativeBoundary.shp") {
  boundary <- read_sf(path) %>%
    filter(!N03_007 %in% TOKYO_ISLANDS) %>%
    select(N03_007)
  st_crs(boundary) <- 4612
  boundary
}

# Osaka census data (2000)
# Download from: https://www.e-stat.go.jp/
load_osaka_census <- function(path = "spatial_R/CENSUS2000/27/h12ka27.shp") {
  read_sf(path) %>%
    mutate(`Population Density` = JINKO / (AREA / 1000000))
}

# Osaka municipal boundary
# Download from: https://nlftp.mlit.go.jp/
load_osaka_boundary <- function(path = "spatial_R/area_municipality/N03-001001_27-g_AdministrativeBoundary.shp") {
  boundary <- read_sf(path) %>%
    select(N03_007)
  st_crs(boundary) <- 4612
  boundary
}

# =============================================================================
# Figure 10: Tokyo Map
# =============================================================================
fig10_tokyo_map <- function(DATA, TOKYO, TOKYO_boundary) {
  # Filter facilities: Tokyo (pref code 13), 2000-2004, excluding islands
  # NOTE: Municipality is numeric, so codes like 01332 become 1332.
  # Use str_count()-3 to extract prefecture code (works for both 4 and 5 digit codes)
  tokyo_facilities <- DATA %>%
    filter(N >= 3) %>%
    filter(str_sub(Municipality, 1, str_count(Municipality) - 3) == "13") %>%
    filter(Year %in% 2000:2004) %>%
    filter(!Municipality %in% TOKYO_ISLANDS_NUM)

  ggplot() +
    geom_sf(
      data = TOKYO,
      aes(fill = `Population Density`),
      color = NA,
      alpha = 0.5
    ) +
    geom_sf(
      data = TOKYO_boundary,
      fill = NA,
      linewidth = 0.7
    ) +
    geom_sf(
      data = tokyo_facilities,
      size = 2
    ) +
    scale_fill_gradientn(colours = map_colors) +
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      style = "ticks",
      unit_category = "metric"
    ) +
    theme_map
}

# =============================================================================
# Figure 11: Osaka Map
# =============================================================================
fig11_osaka_map <- function(DATA, OSAKA, OSAKA_boundary) {
  # Filter facilities: Osaka (pref code 27), 2000-2004
  # NOTE: Municipality is numeric, use str_count()-3 to extract prefecture code
  osaka_facilities <- DATA %>%
    filter(N >= 3) %>%
    filter(str_sub(Municipality, 1, str_count(Municipality) - 3) == "27") %>%
    filter(Year %in% 2000:2004)

  ggplot() +
    geom_sf(
      data = OSAKA,
      aes(fill = `Population Density`),
      color = NA,
      alpha = 0.5
    ) +
    geom_sf(
      data = OSAKA_boundary,
      fill = NA,
      linewidth = 0.7
    ) +
    geom_sf(
      data = osaka_facilities,
      size = 2
    ) +
    scale_fill_gradientn(colours = map_colors) +
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      style = "ticks",
      unit_category = "metric"
    ) +
    theme_map
}

# =============================================================================
# Figure 12: Distance Distribution Histogram with ECDF
# =============================================================================
fig12_distance_histogram <- function(DATA) {
  df <- DATA %>%
    st_drop_geometry() %>%
    drop_na() %>%
    filter(N >= 3)

  max_density <- max(density(df$Distance)$y)

  ggplot(df, aes(x = Distance)) +
    geom_histogram(
      aes(y = after_stat(density)),
      fill = "steelblue",
      color = "white",
      bins = 30
    ) +
    geom_step(
      aes(y = after_stat(ecdf) * max_density),
      stat = "ecdf",
      color = "#d95f02",  # Dark2 orange
      linewidth = 0.8
    ) +
    scale_y_continuous(
      name = "Density",
      limits = c(0, max_density),
      sec.axis = sec_axis(~ . / max_density, name = "ECDF")
    ) +
    labs(x = "Distance to municipal boundary (m)") +
    theme_paper +
    theme(legend.position = "none")
}

# =============================================================================
# Figure 13: Distance vs Relative Density Scatterplot
# =============================================================================
# NOTE: Y-axis uses log1p transformation for statistical validity (handles
# RelDens = 0 cases), but displays original scale values as axis labels.
# Reference line at RelDens = 1 indicates the municipal average density.
fig13_distance_reldens <- function(DATA) {
  df <- DATA %>%
    st_drop_geometry() %>%
    filter(N >= 3)

  # Y-axis: log1p transformation with original scale labels
  breaks_original <- c(0, 0.5, 1, 2, 4, 7)
  breaks_transformed <- log1p(breaks_original)
  labels_display <- c("0", "0.5", "1 (avg)", "2", "4", "7")

  ggplot(df, aes(x = Distance, y = log1p(RelDens))) +
    geom_hline(yintercept = log1p(1), linetype = "dashed",
               color = "gray50", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
    geom_smooth(method = "lm", color = "#d95f02", linewidth = 1) +  # Dark2 orange
    scale_y_continuous(
      breaks = breaks_transformed,
      labels = labels_display,
      name = "Relative Density"
    ) +
    labs(x = "Distance to municipal boundary (m)") +
    theme_paper
}

# =============================================================================
# Descriptive Statistics for Table 1
# =============================================================================
compute_descriptive_stats <- function(DATA) {
  df <- DATA %>%
    st_drop_geometry() %>%
    filter(N >= 3)

  stats <- df %>%
    summarise(
      # Distance
      Distance_mean = mean(Distance, na.rm = TRUE),
      Distance_median = median(Distance, na.rm = TRUE),
      Distance_sd = sd(Distance, na.rm = TRUE),
      Distance_min = min(Distance, na.rm = TRUE),
      Distance_max = max(Distance, na.rm = TRUE),
      # RelDens
      RelDens_mean = mean(RelDens, na.rm = TRUE),
      RelDens_median = median(RelDens, na.rm = TRUE),
      RelDens_sd = sd(RelDens, na.rm = TRUE),
      RelDens_min = min(RelDens, na.rm = TRUE),
      RelDens_max = max(RelDens, na.rm = TRUE),
      # N
      n_obs = n()
    )

  cat("=== Descriptive Statistics for Table 1 ===\n\n")
  cat(sprintf("N = %d\n\n", stats$n_obs))

  cat("Distance (m):\n")
  cat(sprintf("  Mean:   %.2f\n", stats$Distance_mean))
  cat(sprintf("  Median: %.2f\n", stats$Distance_median))
  cat(sprintf("  Std.Dev: %.2f\n", stats$Distance_sd))
  cat(sprintf("  Min:    %.2f\n", stats$Distance_min))
  cat(sprintf("  Max:    %.2f\n\n", stats$Distance_max))

  cat("Relative Density:\n")
  cat(sprintf("  Mean:   %.3f\n", stats$RelDens_mean))
  cat(sprintf("  Median: %.3f\n", stats$RelDens_median))
  cat(sprintf("  Std.Dev: %.3f\n", stats$RelDens_sd))
  cat(sprintf("  Min:    %.3f\n", stats$RelDens_min))
  cat(sprintf("  Max:    %.3f\n", stats$RelDens_max))

  invisible(stats)
}

# =============================================================================
# Generate and Save All Section 4 Figures
# =============================================================================
generate_section4_figures <- function(output_dir = "figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load data
  cat("Loading data...\n")
  DATA <- load_data()

  # Figures 12-13 (no shapefiles needed)
  cat("Generating Figure 12 (distance histogram)...\n")
  p12 <- fig12_distance_histogram(DATA)
  ggsave(file.path(output_dir, "fig12_distance_histogram.pdf"), p12,
         width = 6, height = 4, dpi = 600)
  cat("Saved: fig12_distance_histogram.pdf\n")

  cat("Generating Figure 13 (distance vs relative density)...\n")
  p13 <- fig13_distance_reldens(DATA)
  ggsave(file.path(output_dir, "fig13_distance_reldens.pdf"), p13,
         width = 6, height = 4, dpi = 600)
  cat("Saved: fig13_distance_reldens.pdf\n")

  # Figures 10-11 (require shapefiles)
  cat("\nAttempting to load shapefiles for maps...\n")
  cat("(If paths are incorrect, download data and update paths in load_*() functions)\n\n")

  tryCatch({
    TOKYO <- load_tokyo_census()
    TOKYO_boundary <- load_tokyo_boundary()

    cat("Generating Figure 10 (Tokyo map)...\n")
    p10 <- fig10_tokyo_map(DATA, TOKYO, TOKYO_boundary)
    ggsave(file.path(output_dir, "fig10_tokyo_map.pdf"), p10,
           width = 7, height = 6, dpi = 600)
    cat("Saved: fig10_tokyo_map.pdf\n")
  }, error = function(e) {
    cat("Skipped Figure 10: Tokyo shapefiles not found.\n")
    cat("  Download from: https://www.e-stat.go.jp/ (Census) and https://nlftp.mlit.go.jp/ (Boundary)\n")
  })

  tryCatch({
    OSAKA <- load_osaka_census()
    OSAKA_boundary <- load_osaka_boundary()

    cat("Generating Figure 11 (Osaka map)...\n")
    p11 <- fig11_osaka_map(DATA, OSAKA, OSAKA_boundary)
    ggsave(file.path(output_dir, "fig11_osaka_map.pdf"), p11,
           width = 7, height = 6, dpi = 600)
    cat("Saved: fig11_osaka_map.pdf\n")
  }, error = function(e) {
    cat("Skipped Figure 11: Osaka shapefiles not found.\n")
    cat("  Download from: https://www.e-stat.go.jp/ (Census) and https://nlftp.mlit.go.jp/ (Boundary)\n")
  })

  # Descriptive statistics
  cat("\n")
  compute_descriptive_stats(DATA)

  cat("\nSection 4 figure generation complete.\n")
}

# =============================================================================
# Display Figures (for interactive use)
# =============================================================================
# Uncomment to display:
# DATA <- load_data()
# print(fig12_distance_histogram(DATA))
# print(fig13_distance_reldens(DATA))
# compute_descriptive_stats(DATA)

# =============================================================================
# Generate All Figures
# =============================================================================
# Uncomment to generate:
# generate_section4_figures()
