# Paired T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar
# Simplified Version meant for teaching demos
# This file stores the ggplot functions required for the power curve visualization when power is calculated


# T test alpha stratified power curve
N_alpha_pow_curve_t <- function(N, effect_size, alpha, power, side)
{
  plot_size <- 18
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets){
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 3)) %>%
      arrange(N) %>%
      mutate(alpha_level = rep.int(alpha_presets, length(N)/3),
             power = map2_dbl(N, alpha_level, ~pwr.t.test(n = .x, sig.level = .y, d = effect_size, type = "one.sample", alternative = side)[["power"]]))
  }else{
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 3)) %>%
      arrange(N) %>%
      mutate(alpha_level = rep.int(c(0.01, 0.05, 0.10, alpha), length(N)/4),
             power = map2_dbl(N, alpha_level, ~pwr.t.test(n = .x, sig.level = .y, d = effect_size, type = "one.sample", alternative = side)[["power"]]))
  }
  
  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)
  
  power_data_user <- power_data %>%
    filter(alpha == alpha_level)
  
  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max = max(power),
              min = min(power),
              max_N = max(N),
              .groups = 'drop')
  
  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    geom_segment(aes(x = 0,
                     y = power,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    # Color for non-user entered
    geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user %>% filter, mapping = aes(x = N, y = power, group = alpha_level), color = "#999999") +
    # Color for user entered
    geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N + 1.2, y = max, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N) + 1.2, y = max(power_data_user$power), label = alpha), color = "#E69F00") +
    theme_bw(base_size = plot_size) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: Paired T-test by alpha level",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified sample size and resulting power.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 1))
}

# Z test alpha stratified power curve
N_alpha_pow_curve_z <- function(N, effect_size, alpha, power, side)
{
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets){
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 3)) %>%
      arrange(N) %>%
      mutate(alpha_level = rep.int(alpha_presets, length(N)/3),
             power = map2_dbl(N, alpha_level, ~pwr.norm.test(n = .x, sig.level = .y, d = effect_size, alternative = side)[["power"]]))
  }else{
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 4)) %>%
      arrange(N) %>%
      mutate(alpha_level = rep.int(c(0.01, 0.05, 0.10, alpha), length(N)/4),
             power = map2_dbl(N, alpha_level, ~pwr.norm.test(n = .x, sig.level = .y, d = effect_size, alternative = side)[["power"]]))
  }
  
  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)
  
  power_data_user <- power_data %>%
    filter(alpha == alpha_level)
  
  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max = max(power),
              min = min(power),
              max_N = max(N),
              .groups = "drop")
  
  user_values <- tibble(N = N, power = power)
  
  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    geom_segment(aes(x = 0,
                     y = power,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    # Color for non-user entered
    geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user %>% filter, mapping = aes(x = N, y = power, group = alpha_level), color = "#999999") +
    # Color for user entered
    geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N + 1.2, y = max, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N) + 1.2, y = max(power_data_user$power), label = alpha), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: Paired Z-test by alpha level",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified sample size and resulting power.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 1))
}

# T test effect size stratified power curve
N_effect_pow_curve_t <- function(N, effect_size, alpha, power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 3)) %>%
      arrange(N) %>%
      mutate(effect_val = rep.int(std_effect, length(N)/3),
             power = map2_dbl(N, effect_val, ~pwr.t.test(n = .x, sig.level = alpha, d = .y, type = "one.sample", alternative = side)[["power"]]))
  }else{
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 4)) %>%
      arrange(N) %>%
      mutate(effect_val = rep.int(c(0.2, 0.5, 0.8, effect_size), length(N)/4),
             power = map2_dbl(N, effect_val, ~pwr.t.test(n = .x, sig.level = alpha, d = .y, type = "one.sample", alternative = side)[["power"]]))
  }
  
  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)
  
  power_data_user <- power_data %>%
    filter(effect_size == effect_val)
  
  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max = max(power),
              min = min(power),
              max_N = max(N),
              .groups = "drop")
  
  user_values <- tibble(N = N, power = power)
  
  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    geom_segment(aes(x = 0,
                     y = power,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    # Color for non-user entered
    geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user, mapping = aes(x = N, y = power, group = effect_val), color = "#999999") +
    # Color for user entered
    geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N + 1.2, y = max, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N) + 1.2, y = max(power_data_user$power), label = effect_size), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: Paired T-test by effect size",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified sample size and resulting power.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 1))
}

# Z test effect size stratified power curve
N_effect_pow_curve_z <- function(N, effect_size, alpha, power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 3)) %>%
      arrange(N) %>%
      mutate(effect_val = rep.int(std_effect, length(N)/3),
             power = map2_dbl(N, effect_val, ~pwr.norm.test(n = .x, sig.level = alpha, d = .y, alternative = side)[["power"]]))
  }else{
    power_data <- tibble(N = rep.int(seq(2, ifelse(N < 45, N + 5, 50), 1), 4)) %>%
      arrange(N) %>%
      mutate(effect_val = rep.int(c(0.2, 0.5, 0.8, effect_size), length(N)/4),
             power = map2_dbl(N, effect_val, ~pwr.norm.test(n = .x, sig.level = alpha, d = .y, alternative = side)[["power"]]))
  }
  
  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)
  
  power_data_user <- power_data %>%
    filter(effect_size == effect_val)
  
  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max = max(power),
              min = min(power),
              max_N = max(N),
              .groups = 'drop')
  
  user_values <- tibble(N = N, power = power)
  
  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    geom_segment(aes(x = 0,
                     y = power,
                     xend = N,
                     yend = power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    # Color for non-user entered
    geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user, mapping = aes(x = N, y = power, group = effect_val), color = "#999999") +
    # Color for user entered
    geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N + 1.2, y = max, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N) + 1.2, y = max(power_data_user$power), label = effect_size), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: Paired Z-test by effect size",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified sample size and resulting power.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 1))
}

# ---------------------------------------- CODE BELOW IS FOR TESTING PURPOSES--------------------------------------------
# power <- -1
# N <- -1
# mu0 <- 0
# muA <- 1
# sd <- 1
# two_sided <- 2
# effect_size <- abs(mu0 - muA)/sd
# which_calc <- 2
# sd_known <- TRUE
# N <- 20
# power <-  0.80
# alpha <- 0.05
# effect_size <- abs(mu0 - muA)/sd
# alternative <- c("greater", "two.sided")
# pow_curve <- 1
# 
# if(as.numeric(which_calc) == 1){
#   power <- ifelse(sd_known == FALSE,
#                   pwr.t.test(n = N,
#                              d = effect_size,
#                              sig.level = alpha,
#                              alternative = alternative[[two_sided]])[["power"]],
#                   pwr.norm.test(n = N,
#                                 d = effect_size,
#                                 sig.level = alpha,
#                                 alternative = alternative[[two_sided]])[["power"]])
#   N <- N
# } else {
#   N <- ifelse(sd_known == FALSE,
#               pwr.t.test(d = effect_size,
#                          sig.level = alpha,
#                          power = power,
#                          alternative = alternative[[two_sided]])[["n"]],
#               pwr.norm.test(d = effect_size,
#                             sig.level = alpha,
#                             power = power,
#                             alternative = alternative[[two_sided]])[["n"]])
#   N <- ceiling(N)
#   power <- power
# }
# if(pow_curve == 2)
# {
#   # Alpha case
#   if(sd_known == FALSE){
#     # Student's T
#     alpha_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#   } else{
#     # Normal
#     alpha_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#   }
# } else{
#   # Effect size case
#   if(sd_known == FALSE){
#     # Student's T
#     effect_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#   } else{
#     # Normal
#     effect_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#   }
# }
