library(data.table)
library(tidyverse)

# Adapt the paths as needed
inds_fixed_emig <- fread(glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples/dispersal/Outputs/Batch3_Sim1_Land1_Rep0_Inds.txt"
))
inds_evol_emig <- fread(glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples//Outputs/Batch3_Sim2_Land1_Rep0_Inds.txt"
))


ggplot(inds_fixed_emig, aes(x = Year, y = Y)) +
    ggplot2::geom_jitter(alpha = 0.4, width = 10, colour = "#022a5b") +
    scale_colour_viridis_c() +
    ylim(c(0, 800))

ggplot(inds_evol_emig, aes(x = Year, y = Y, colour = EP)) +
    ggplot2::geom_jitter(alpha = 0.4, width = 10) +
    scale_colour_viridis_c() +
    ylim(c(0, 800))
