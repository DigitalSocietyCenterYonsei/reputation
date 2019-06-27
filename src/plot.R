#!/usr/bin/env Rscript
# Re-process data and generate plots
# Author: jongbin

library("tidyverse")
library("readxl")

theme_set(theme_bw())

DATA_DIR <- normalizePath("../data")
PLOT_DIR <- normalizePath("../figs", mustWork = FALSE)
TARGET_DIR <- file.path(DATA_DIR, "fig")

# Methods to highlight in plots
MAIN_METHODS <- c("BSD_SR", "BSD")

# Helper functions --------------------------------------------------------
check_path <- function (path) {
  # Function to check whether directory of path exists, and if not, create
  # Always returns the final path, so it can be chained in other save methods
  # for safe-saving
  dirpath <- dirname(path)
  if (!dir.exists(dirpath)) {
    message(sprintf("Creating path\n\t%s", dirpath))
    dir.create(dirpath, recursive = TRUE)
  }

  path
}

# PLOT: Baseline comparison -----------------------------------------------
results_df <- file.path(DATA_DIR, "fig_4.xlsx") %>%
  read_excel() %>%
  gather(method, rho, BL, RS)

p_results <- ggplot(results_df, aes(x = method, y = rho)) +
  geom_boxplot() +
  scale_y_continuous("Rank correlation\n", limits = c(.8, .95)) +
  scale_x_discrete("\nMethod",
                   breaks = c("BL", "RS"),
                   labels = c("Baseline", "Rating separation")) +
  facet_wrap(~ set, labeller = labeller(
      "set" = function(s) paste("Parameter set", s))
  ) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))
ggsave(file.path(PLOT_DIR, "initial_results.pdf") %>% check_path,
       plot = p_results,
       width = 7, height = 5)

# PLOT: Iteration evolution -----------------------------------------------
iters_df <- file.path(DATA_DIR, "fig_5.xlsx") %>%
  read_excel()

p_iters <- ggplot(iters_df, aes(x = iter, y = rho)) +
  geom_line() +
  geom_point() +
  scale_y_continuous("Rank correlation\n") +
  scale_x_continuous("\nIteration", breaks = 1:10) +
  facet_wrap(~ set, labeller = labeller(
      "set" = function(s) paste("Parameter set", s))
  ) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14))
ggsave(file.path(PLOT_DIR, "iterations.pdf") %>% check_path,
       plot = p_iters,
       width = 7, height = 3)

# PLOT: Detailed results for attack types ---------------------------------
original_files <- list.files(file.path(DATA_DIR, "fig_6-11"),
                             full.names = TRUE)

attacks_df <- map_dfr(original_files, function(path) {
    method <- tools::file_path_sans_ext(basename(path))
    read_excel(path, skip = 1) %>%
      mutate(method = method)
  }) %>%
  gather(attack_rate, rho, `10`:`90`) %>%
  mutate(method_type = if_else(method %in% MAIN_METHODS, "main", "bench"),
         method = fct_reorder(factor(method), -rho),
         pattern = paste("Pattern", pattern),
         attack_rate = as.numeric(attack_rate) / 100)

p_attacks <- ggplot(attacks_df, aes(x = attack_rate, y = rho, group = method)) +
  geom_hline(yintercept = 0, size = 1.5, color = "grey92") +
  geom_line(aes(color = method, linetype = method)) +
  # geom_line(aes(color = method, linetype = method_type)) +
  scale_x_continuous("\nRate of attack",
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(.1, .9), breaks = seq(.1, .9, .2)) +
  scale_y_continuous("Rank correlation\n",
                     labels = scales::number_format(0.1)) +
  scale_color_brewer(
      "Method",
      palette = "Dark2",
      limits = c("BSD", "BSD_SR", "BL", "BRS", "PA", "ICLUB"),
      breaks = c("BSD", "BSD_SR", "BL", "BRS", "PA", "ICLUB"),
      labels = c("IW", "RS&IW", "Baseline", "BRS", "PA", "iCLUB")
  ) +
  scale_linetype_manual(
      "Method",
      limits = c("BSD", "BSD_SR", "BL", "BRS", "PA", "ICLUB"),
      values = c("solid", "solid", "53", "22", "22", "22"),
      breaks = c("BSD", "BSD_SR", "BL", "BRS", "PA", "ICLUB"),
      labels = c("IW", "RS&IW", "Baseline", "BRS", "PA", "iCLUB")
  ) +
  facet_grid(pattern ~ scheme, scales = "free_y") +
  theme(panel.grid.minor = element_blank(),
        panel.spacing.x = unit(3, "mm"),
        panel.spacing.y = unit(2, "mm"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 12))
ggsave(file.path(PLOT_DIR, "attack_results.pdf") %>% check_path,
       plot = p_attacks,
       width = 8, height = 8)
