source("~/P001/scripts/plot_theme_custom.R")
pacman::p_load(ggplot2, tidyverse, latex2exp, scales)


# Parameters --------------------------------------------------------------

N <- 100

mu_phi <- 0.25
sigma_phi <- 0.122

mu_eps <- 0
sigma_eps_vec <- c(0, 20, 40)

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
  
  tibble(
    sigma_eps = sigma_x,
    threshold_vec = threshold_vec,
    threshold_count = findInterval(threshold_vec, thresholds),
    activation_count = {
      if (threshold_vec[1] == 0L && threshold_count[1] == 0L) {
        rep.int(0L, length(threshold_vec))
      } else{
        cumall(threshold_count >= threshold_vec) %>%
          as.integer() %>%
          `*`(threshold_count) %>%
          cummax()
      }
    }
  )
}

run_simulation <- function(sigma_x, iterations, seed) {
  set.seed(seed)
  sigma_x %>%
    map(\(sx) {
      iterations %>%
        map(\(i) run_model(sx)) %>%
        list_rbind()
    }) %>%
    list_rbind()
}


# Data --------------------------------------------------------------------


time_acti_data <- run_simulation(sigma_eps_vec, iterations, seed)

# saveRDS(time_acti_data, file = "./P001/data/time_acti_data.rds")
# time_acti_data <- readRDS("./P001/data/time_acti_data.rds")


# Plot --------------------------------------------------------------------


time_acti_plot <-
  time_acti_data %>%
  summarise(
    mean_acti = mean(activation_count),
    sd_acti   = sd(activation_count) / sqrt(n()),
    .by = c(sigma_eps, threshold_vec)
  ) %>%
  ggplot(., aes(
    threshold_vec,
    mean_acti,
    color = factor(sigma_eps),
    fill  = factor(sigma_eps)
  )) +
  geom_ribbon(aes(
    ymin = pmax(0, mean_acti - 1.96 * sd_acti),
    ymax = pmin(100, mean_acti + 1.96 * sd_acti)
  ),
  alpha = 0.15,
  colour = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_color_manual(values = c("#CC3311", "#009944", "#4477AA")) +
  scale_fill_manual(values  = c("#CC3311", "#009944", "#4477AA")) +
  labs(
    x = "Time Step",
    y = "Activation Level (Individuals)",
    color = expression(sigma[epsilon]),
    fill = expression(sigma[epsilon])
  ) +
  theme_custom


ggsave(
  file.path("~/P001/images/", "3_time_acti_plot.pdf"),
  time_acti_plot,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)



# Old version -------------------------------------------------------------
#
# thres_fun <- function(sigma_x) {
#   phi  <- rnorm(N, mean = mu_phi, sd = sigma_phi)
#   eps  <- rnorm(N, mean = mu_eps, sd = sigma_x)
#
#   n_i_prime <- (phi * (N + eps)) %>%
#     scales::squish(., c(0, 100)) %>%
#     cut(.,
#         breaks = n_t,
#         include.lowest = TRUE,
#         right = FALSE) %>%
#     table() %>%
#     as_tibble(.name_repair = ~ c("time_step", "count")) %>%
#     transform(sigma_eps = sigma_x)
#
#   return(n_i_prime)
#
# }
#
# sigma_eps_data <- map_dfr(sigma_eps, \(sigma_x) {
#   map_dfr(seq_len(iterations), ~ thres_fun(sigma_x), .id = "iteration_id")
# }) %>%
#   mutate(lower_bound = as.numeric(str_extract(time_step, "(?<=\\[|\\().+?(?=,)")),
#          upper_bound = as.numeric(str_extract(time_step, "(?<=,).+?(?=\\]|\\))"))) %>%
#   mutate(
#     cum_count = cumsum(count),
#     activation = {
#       cc <- cum_count
#       ub <- upper_bound
#       n  <- length(cc)
#
#       k <- which(cc < ub)[1]
#
#       if (is.na(k)) {
#         cc
#       } else {
#         c(cc[seq_len(k - 1L)], if (k == 1L)
#           0
#           else
#             cc[k - 1L], rep(cc[k], n - k))
#       }
#     },
#     .by =  c(sigma_eps, iteration_id),
#     .after = c(count)
#   ) %>%
#   summarise(
#     final_time_step   = dplyr::last(time_step),
#     final_upper_bound = dplyr::last(upper_bound),
#     final_activation  = dplyr::last(activation),
#     .by = c(sigma_eps, iteration_id)
#   )
