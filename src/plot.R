#!/usr/bin/env Rscript
# Re-process data and generate plots
# Author: jongbin

library("tidyverse")
library("readxl")

theme_set(theme_bw())

DATA_DIR <- normalizePath("../data")
PLOT_DIR <- normalizePath("../figs", mustWork = FALSE)

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

# PLOT: Detailed results for attack types ---------------------------------
TARGET_DIR <- file.path(DATA_DIR, "fig")
original_files <- list.files(file.path(DATA_DIR, "fig_6-11"), full.names = TRUE)

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
  geom_line(aes(color = method, linetype = method_type)) +
  scale_x_continuous("\nRate of attack",
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(.1, .9), breaks = seq(.1, .9, .2)) +
  scale_y_continuous("Rank correlation\n") +
  scale_linetype_manual(limits = c("bench", "main"),
                        values = c("22", "solid"), guide = FALSE) +
  facet_grid(pattern ~ scheme, scales = "free_y")

ggsave(file.path(PLOT_DIR, "attack_results.pdf") %>% check_path,
       plot = p_attacks,
       width = 8, height = 8)
