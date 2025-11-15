source("~/P001/scripts/plot_theme_custom.R")
pacman::p_load(ggplot2, tidyverse, latex2exp)


# Parameters --------------------------------------------------------------

N <- 100

mu_phi <- 0.25
sigma_phi <- 0.122

mu_eps <- 0
sigma_eps_vec <- c(0, 20, 40)

iterations <- 500
seed <- 123

n_grid <- 512



# Function ----------------------------------------------------------------

run_model <- function(sigma_x) {
  phi <- rnorm(N, mu_phi, sigma_phi)
  eps <- rnorm(N, mu_eps, sigma_x)
  
  thresholds <- phi * (N + eps)
  
  threshold_distribution <- density(thresholds,
                                    from = 0,
                                    to = 100,
                                    n = n_grid)
  
  tibble(
    sigma_eps = sigma_x,
    threshold_value = threshold_distribution$x,
    threshold_density = threshold_distribution$y
  )
}


run_simulation <- function(sigma_x, seed, n_grid) {
  set.seed(seed)
  xs <- seq(0, 100, length.out = n_grid)
  
  sigma_x %>%
    map(\(sx) {
      replicate(iterations, run_model(sx), simplify = FALSE) %>%
        bind_rows()
    }) %>%
    list_rbind()
}


# Data --------------------------------------------------------------------


thres_kde_data <- run_simulation(sigma_eps_vec, seed, n_grid)


# Plot --------------------------------------------------------------------

thres_kde_plot <-
  thres_kde_data %>%
  summarise(
    density = mean(threshold_density),
    .by = c(sigma_eps, threshold_value)
  ) %>%
  ggplot(aes(threshold_value, density, color = factor(sigma_eps))) +
  geom_line(linewidth = 1) +
  labs(
    x = expression(tilde(n)[i] == varphi[i] * (N + epsilon[i])),
    y = expression("Probability Density of" ~ tilde(n)[i]),
    color = expression(sigma[epsilon])
  ) +
  scale_color_manual(values = c("#CC3311", "#009944", "#4477AA")) +
  theme_custom


ggsave(
  filename = file.path("~/P001/images/", "2_thres_kde_plot.pdf"),
  plot = thres_kde_plot,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)

