# Plotting gamma and scaled distributions

# Load the functions below and use the plotting snippet on the last line
# to visualise how the parameters change the distributions

# Gamma distribution is used to sample selection coefficients (allele values)
# based on a shape and a scale parameter.
#  Mean = shape * scale

# "Scaled" distribution sets values of the dominance coefficients
# based on the values sampled for the selection coefficients
# High dominance coefficients are more likely to be sampled for low-effect mutations
# High-effect mutations (lethal alleles) are more likely to be recessive

# Bear in mind: the plots show the range of possible values for *one* allele,
# if you're modelling e.g. 50 diploid loci the average genetic load will be
# roughly shape * scale * 50 * 2

library(tidyverse)

plot_gamma <- function(shape, scale, xlim = c(0, 0.001)) {
    tibble::tibble(
        s = seq(0, 1, 0.00001),
        density  = stats::dgamma(s, shape = shape, scale = scale)
    ) %>%
        dplyr::mutate("density" = density / sum(density)) %>%
        ggplot2::ggplot(ggplot2::aes(x = s, y = density)) +
        ggplot2::geom_line() +
        ggplot2::coord_cartesian(xlim = xlim)
}

get_scaled_h <- function(s, sd, hd) {
    return(exp(-s*(-log(2*hd)/sd)))
}

plot_gamma_with_h <- function(shape, scale, hd, xlim = c(0, 0.001)) {

    sd <- shape * scale

    coeff <- 10 # used to plot s and h on the same scale

    data_tbl <- tibble::tibble(
        s = seq(0, 1, 0.00001),
        p = dgamma(s, shape = shape, scale = scale)
    ) %>%
        dplyr::mutate(
            "p" = p / sum(p),
            "h" = exp(-s * (-log(2 * hd) / sd))
        )

    data_tbl %>%
        ggplot2::ggplot(ggplot2::aes(s, p)) +
        ggplot2::geom_line() +
        # Add dominance coeffs values to the plot
        ggplot2::geom_line(ggplot2::aes(y = h / coeff), colour = "red") +
        ggplot2::scale_y_continuous(
            name = "Density",
            sec.axis = ggplot2::sec_axis(~.*coeff, name = "Dominance coeff (h)")
        ) +
        ggplot2::xlab("Selection coeff (s)") +
        ggplot2::coord_cartesian(xlim = xlim)

}

plot_gamma_with_h(
    shape = 1, # shape parameter of the gamma
    scale = 0.2, # scale parameter
    hd = 0.2, # mean parameter of the dominance distribution
    xlim = c(0, 1) # use this to zoom in or out
)

# Gamma alone
plot_gamma(
    shape = 1,
    scale = 0.2,
    xlim = c(0, 1)
)
