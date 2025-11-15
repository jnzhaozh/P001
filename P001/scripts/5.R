source("~/P001/scripts/plot_theme_custom.R")
pacman::p_load(ggplot2, tidyverse, latex2exp)


# Parameters --------------------------------------------------------------

N <- 100

mu_phi <- .25
sigma_phi <- .122

mu_phi_vec <- seq(0, 1, length.out = 101)
sigma_phi_vec <- seq(0, 1, length.out = 101)

mu_eps <- 0
sigma_eps_vec <- seq(0, 100, length.out = 101)

threshold_vec <- seq(0, 100, length.out = 101)

iterations <- seq_len(500)
seed <- 123


# Function ----------------------------------------------------------------

run_model <- function(sigma_x, mu_y, sigma_y) {
  phi  <- rnorm(N, mean = mu_y, sd = sigma_y)
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


run_simulation <- function(sigma_x, mu_y, sigma_y, iterations, seed) {
  set.seed(seed)
  
  tidyr::expand_grid(sigma_eps = sigma_x,
                     mu_phi    = mu_y,
                     sigma_phi = sigma_y) %>%
    pmap(\(sigma_eps, mu_phi, sigma_phi) {
      act <- vapply(iterations,
                    \(i)
                    run_model(sigma_eps, mu_phi, sigma_phi),
                    numeric(1))
      
      tibble(
        sigma_eps = sigma_eps,
        mu_phi    = mu_phi,
        sigma_phi = sigma_phi,
        activation_count = act
      )
    }) %>%
    list_rbind()
}


# Data --------------------------------------------------------------------

tictoc::tic()
phi_mu_vec_data <- run_simulation(
  sigma_x = sigma_eps_vec,
  mu_y    = mu_phi_vec,
  sigma_y = sigma_phi,
  iterations = iterations,
  seed = seed
)
tictoc::toc()

tictoc::tic()
phi_sigma_vec_data <- run_simulation(
  sigma_x = sigma_eps_vec,
  mu_y    = mu_phi,
  sigma_y = sigma_phi_vec,
  iterations = iterations,
  seed = seed
)
tictoc::toc()


# saveRDS(eps_acti_data, file = "~/P001/data/phi_mu_vec_data.rds")
# phi_mu_vec_data <- readRDS("./P001/data/phi_mu_vec_data.rds")
#
# saveRDS(eps_acti_data, file = "~/P001/data/phi_sigma_vec_data.rds")
# phi_sigma_vec_data <- readRDS("./P001/data/phi_sigma_vec_data.rds")


eps_phi_data <- bind_rows(
  phi_mu_vec_data %>%
    mutate(case = "phi_mu", .before = 1),
  phi_sigma_vec_data %>%
    mutate(case = "phi_sigma", .before = 1)
)


# Plot --------------------------------------------------------------------

eps_phi_plot <-
  eps_phi_data %>%
  summarise(
    activation_count = mean(activation_count),
    .by = c(case, sigma_eps, mu_phi, sigma_phi)
  ) %>%
  mutate(
    case = factor(case, levels = c("phi_mu", "phi_sigma")),
    y_value = if_else(case == "phi_mu", mu_phi, sigma_phi),
    activation_count = activation_count
  ) %>%
  ggplot(., aes(x = sigma_eps, y = y_value, fill = activation_count)) +
  geom_raster() +
  facet_grid(
    rows = vars(case),
    scales = "free_y",
    labeller = labeller(case = as_labeller(
      c(phi_mu = "Vary~mu[varphi]*';'~Fix~sigma[varphi]", phi_sigma = "Fix~mu[varphi]*';'~Vary~sigma[varphi]"),
      default = label_parsed
    ))
  ) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, 100),
    breaks = c(0, 50, 100),
    name = expression("Activation Level (Individuals)")
  ) +
  labs(
    x = expression(sigma[epsilon]),
    y = expression(mu[varphi] * "/" * sigma[varphi])
  ) +
  # coord_cartesian(expand = FALSE) +
  theme_custom


ggsave(
  file.path("~/P001/images/", "5_eps_phi_plot.pdf"),
  eps_phi_plot,
  width = 7,
  height = 8.5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)



# Old version -------------------------------------------------------------

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
#
#
# thres_fun <- function(mu_x, sigma_x, sigma_y) {
#   phi  <- rnorm(N, mean = mu_x, sd = sigma_x)
#   eps  <- rnorm(N, mean = mu_eps, sd = sigma_y)
#
#   n_i_prime <- (phi * (N + eps)) %>%
#     scales::squish(., c(0, 100)) %>%
#     cut(.,
#         breaks = n_t,
#         include.lowest = TRUE,
#         right = FALSE) %>%
#     table() %>%
#     as_tibble(.name_repair = ~ c("time_step", "count")) %>%
#     transform(sigma_eps = sigma_y)
#
#   return(n_i_prime)
#
# }
#
#
# acti_fun <- function(df) {
#   df %>%
#     mutate(lower_bound = as.numeric(str_extract(time_step, "(?<=\\[|\\().+?(?=,)")),
#            upper_bound = as.numeric(str_extract(time_step, "(?<=,).+?(?=\\]|\\))"))) %>%
#     arrange(sigma_eps, iteration_id, upper_bound) %>%
#     mutate(
#       cum_count = cumsum(count),
#       activation = {
#         cc <- cum_count
#         ub <- upper_bound
#         k  <- which(cc < ub)[1]
#         if (is.na(k)) {
#           cc
#         } else {
#           c(cc[seq_len(k - 1L)], if (k == 1L)
#             0
#             else
#               cc[k - 1L], rep(cc[k], length(cc) - k))
#         }
#       },
#       .by = c(sigma_eps, iteration_id),
#       .after = count
#     ) %>%
#     summarise(
#       final_activation = dplyr::last(activation),
#       .by = c(sigma_eps, iteration_id)
#     )
# }
#
#
# eval_fun <- function(mu_x, sigma_x, sigma_y, iterations) {
#   map_dfr(seq_len(iterations),
#           ~ thres_fun(mu_x, sigma_x, sigma_y),
#           .id = "iteration_id") %>%
#     acti_fun() %>%
#     summarise(mean_final = mean(final_activation), .by = sigma_eps) %>%
#     pull(mean_final)
# }
#
#
# mu_phi_val_fun <- function(mu_x, sigma_x, sigma_y, iterations) {
#   tidyr::crossing(mu_phi = mu_x, sigma_eps = sigma_y) %>%
#     mutate(
#       mean_final = purrr::map2_dbl(mu_phi, sigma_eps, ~ eval_fun(.x, sigma_x, .y, iterations)),
#       panel = "Vary μφ (σφ fixed)"
#     ) %>%
#     rename(x = sigma_eps, y = mu_phi)
# }
#
#
# sigma_phi_val_fun <- function(mu_x, sigma_x, sigma_y, iterations) {
#   tidyr::crossing(sigma_phi = sigma_x, sigma_eps = sigma_y) %>%
#     mutate(
#       mean_final = purrr::map2_dbl(sigma_phi, sigma_eps, ~ eval_fun(mu_x, .x, .y, iterations)),
#       panel = "Vary σφ (μφ fixed)"
#     ) %>%
#     rename(x = sigma_eps, y = sigma_phi)
# }
#
# mu_phi_data <- mu_phi_val_fun(mu_phi_val, sigma_phi_fix, sigma_eps, iterations)
# sigma_phi_data <- sigma_phi_val_fun(mu_phi_fix, sigma_phi_val, sigma_eps, iterations)
