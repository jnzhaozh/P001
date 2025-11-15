source("~/P001/scripts/plot_theme_custom.R")
pacman::p_load(ggplot2, tidyverse, latex2exp)


# Parameters --------------------------------------------------------------

N <- 100
phi_i <- 0.5

mu_eps <- 0
sigma_eps_vec <- c(0, 20, 40)

threshold_vec <- seq(0, 100, length.out = 101)

iterations <- 500
seed <- 123



# Function ----------------------------------------------------------------


run_model <- function(sigma_x) {
  eps <- rnorm(N, mean = mu_eps, sd = sigma_x)
  threshold_i <- (eps + N) * phi_i
  
  tibble(
    sigma_eps = sigma_x,
    threshold_vec = threshold_vec,
    # threshold_prob = map(threshold_vec, \(tv) mean(tv >= threshold_i))
    threshold_prob = ecdf(threshold_i)(threshold_vec)
  )
}

run_simulation <- function(sigma_x, seed) {
  set.seed(seed)
  sigma_x %>%
    map(\(sx) {
      replicate(iterations, run_model(sx), simplify = FALSE) %>%
        bind_rows()
    }) %>%
    list_rbind()
}


# Data --------------------------------------------------------------------


thres_prob_data <- run_simulation(sigma_eps_vec, seed)


# Plot --------------------------------------------------------------------

thres_prob_plot <-
  thres_prob_data %>%
  summarise(
    threshold_prob = mean(threshold_prob),
    .by = c(sigma_eps, threshold_vec)
  ) %>%
  ggplot(aes(
    x = threshold_vec,
    y = threshold_prob,
    color = factor(sigma_eps)
  )) +
  geom_line(linewidth = 1) +
  labs(
    x = expression(tilde(n)[i] == varphi[i] * (N + epsilon[i])),
    y = expression("Probability of" ~ tilde(n)[i]),
    color = expression(sigma[epsilon])
  ) +
  scale_color_manual(values = c("#CC3311", "#009944", "#4477AA")) +
  theme_custom



ggsave(
  file.path("~/P001/images/", "1_thres_prob_plot.pdf"),
  thres_prob_plot,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)
