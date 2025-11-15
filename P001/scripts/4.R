source("~/P001/scripts/plot_theme_custom.R")
pacman::p_load(ggplot2, tidyverse, latex2exp, data.table)


# Parameters --------------------------------------------------------------

N <- 100

mu_phi <- 0.25
sigma_phi <- 0.122

mu_eps <- 0
sigma_eps_vec <- seq(0, 100, legnth.out = 101)

threshold_vec <- seq(0, 100, length.out = 101)

iterations <- seq_len(500)
seed <- 123



# Function ----------------------------------------------------------------

run_model <- function(sigma_x) {
  phi  <- rnorm(N, mean = mu_phi, sd = sigma_phi)
  eps  <- rnorm(N, mean = mu_eps, sd = sigma_x)
  
  thresholds <- (phi * (N + eps)) %>%
    pmin(., 100) %>%
    sort.int()
  
  threshold_count <-  findInterval(threshold_vec, thresholds)
  
  activation_count <- {
    if (threshold_vec[1] == 0L && threshold_count[1] == 0L) {
      0L
    } else{
      cumall(threshold_count >= threshold_vec) %>%
        which() %>%
        last() %>%
        threshold_count[.]
    }
  }
  
  return(activation_count)
}


run_simulation <- function(sigma_x, iterations, seed) {
  set.seed(seed)
  
  lapply(sigma_x, \(sx) {
    act <- vapply(iterations, \(i)
                  run_model(sx), numeric(1))
    
    tibble(sigma_eps = sx, activation_count = act)
  }) %>%
    list_rbind()
  
}

# Data --------------------------------------------------------------------

eps_acti_data <- run_simulation(sigma_eps_vec, iterations, seed)

# saveRDS(eps_acti_data, file = "~/P001/data/eps_acti_data.rds")
# eps_acti_data <- readRDS("~/P001/data/eps_acti_data.rds")


# Plot --------------------------------------------------------------------

eps_acti_plot <-
  eps_acti_data %>%
  summarise(
    mean_acti = mean(activation_count),
    sd_acti   = sd(activation_count),
    .by = sigma_eps
  ) %>%
  ggplot(., aes(x = sigma_eps)) +
  geom_line(aes(y = mean_acti, linetype = "Mean"), linewidth = 1) +
  geom_line(aes(y = sd_acti, linetype = "Standard Deviation"), linewidth = 1) +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = c("Mean", "Standard Deviation")
  ) +
  labs(
    x = expression(sigma[epsilon]),
    y = "Activation Level (Individuals)",
    linetype = expression("Metrics")
  ) +
  theme_custom


ggsave(
  file.path("~/P001/images/", "4_eps_acti_plot.pdf"),
  eps_acti_plot,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)



# Old version -------------------------------------------------------------
#
# run_model <- function(sigma_x) {
#   phi  <- rnorm(N, mean = mu_phi, sd = sigma_phi)
#   eps  <- rnorm(N, mean = mu_eps, sd = sigma_x)
#   thresholds <- (phi * (N + eps)) %>%
#     pmin(., 100) %>%
#     sort.int()
#
#   tibble(
#     sigma_eps = sigma_x,
#     threshold_vec = threshold_vec,
#     threshold_count = findInterval(threshold_vec, thresholds),
#     activation_count = {
#       if (threshold_vec[1] == 0L && threshold_count[1] == 0L) {
#         rep.int(0L, length(threshold_vec))
#       } else{
#         cumall(threshold_count >= threshold_vec) %>%
#           as.integer() %>%
#           `*`(threshold_count) %>%
#           cummax()
#       }
#     }
#   ) %>%
#     last()
# }
#
#
# run_simulation <- function(sigma_x, iterations, seed) {
#   set.seed(seed)
#   sigma_x %>%
#     map(\(sx) {
#       seq_len(iterations) %>%
#         map(\(i) run_model(sx)) %>%
#         list_rbind(names_to = "iteration") %>%
#         mutate(iteration = as.integer(iteration))
#     }) %>%
#     list_rbind()
# }
#
