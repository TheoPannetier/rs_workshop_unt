library(data.table)
library(tidyverse)

# Adapt the paths as needed
path_no_shift <- glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples/genetic_load/expansion_load_no_shift_inds.txt"
)
path_shift <- glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples/genetic_load/expansion_load_with_shift_inds.txt"
)

# Read and collate data
ind_tbl <- dplyr::bind_rows(
    fread(path_no_shift),
    fread(path_shift),
    .id = "shift"
) %>%
    dplyr::mutate("shift" = shift == 2) # convert to logical

ind_tbl <- ind_tbl[, .(Year, Y, ProbViable, shift)]

# Compare mean genetic load
ind_tbl %>%
    group_by(Year, shift) %>%
    summarise("fitness" = mean(ProbViable)) %>%
    ggplot(aes(Year, fitness, colour = shift)) +
    geom_line() + geom_point() +
    ylim(c(0.85,1)) +
    xlim(c(0, 1000)) +
    labs(y = "Mean fitness", colour = "Range shift")

# (Shift only)
# Fitness vs Y-coord over time
# is there expansion load?
fread(path_shift) %>%
    ggplot(aes(x = Y, y = ProbViable)) +
    geom_line() +
    labs(y = "Mean fitness") +
    facet_wrap(vars(Year), scales = "free_x", labeller = label_both)

