library(data.table)
library(tidyverse)

# Adapt the paths as needed
path_no_shift <- glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples/Outputs/Batch1_Sim1_Land1_neutralGenetics.txt"
)
path_shift <- glue::glue(
    "C:/Users/s02tp3/github/rs_worskhop_unt/batch_examples/Outputs/Batch1_Sim2_Land1_neutralGenetics.txt"
)

# Read and collate data
neutral_tbl <- dplyr::bind_rows(
    fread(path_no_shift),
    fread(path_shift),
    .id = "shift"
) %>%
    dplyr::mutate("shift" = shift == 2) # convert to logical

neutral_tbl %>%
    ggplot(aes(Year, meanFixedLociPatches, colour = shift)) +
    geom_line() +
    geom_point()# +
    #labs(y = "Ho", colour = "Range shift")# +
    #ylim(c(0, 1))

# Genotypes

genes_tbl <- fread(
    "C:/Users/s02tp3/Desktop/ws/neutral/Outputs/Batch1_Sim2_Land1_Rep0_geneValues.txt"
) %>%
    dplyr::select(Year, IndID, locusPosition:alleleValueA) # free up memory


genes_tbl %>%
    # Calc genotype frequencies
    group_by(Year, alleleValueA) %>%
    count() %>%
    ungroup(alleleValueA) %>%
    mutate("f" = n / sum(n)) %>%
    # Plot
    ggplot(aes(x = Year, y = f, fill = as.factor(alleleValueA))) +
    geom_col(show.legend = FALSE)



