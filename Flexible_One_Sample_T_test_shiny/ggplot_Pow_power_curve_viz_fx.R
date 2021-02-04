# One-Sample T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar
# Flexible version to allow for calculation of most values
# This file stores the ggplot functions required for the power curve visualization when N is calculated

# T test alpha stratified power curve
Pow_alpha_pow_curve_t <- function(N, effect_size, alpha, user_power, side)
{
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets){
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, 0.99), 0.01), 3)) %>%
      arrange(power) %>%
      mutate(alpha_level = rep.int(alpha_presets, length(power)/3),
             N = map2_dbl(power, alpha_level, ~ceiling(pwr.t.test(power = .x, sig.level = .y, d = effect_size, type = "one.sample", alternative = side)[["n"]])))
  }else{
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, user_power), 0.01), 4)) %>%
      arrange(power) %>%
      mutate(alpha_level = rep.int(c(0.01, 0.05, 0.10, alpha), length(power)/4),
             N = map2_dbl(power, alpha_level, ~ceiling(pwr.t.test(power = .x, sig.level = .y, d = effect_size, type = "one.sample", alternative = side)[["n"]])))
  }
  
  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)
  
  power_data_user <- power_data %>%
    filter(alpha == alpha_level)
  
  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max_power = max(power),
              min_power = min(power),
              max_N = max(N),
              .groups = 'drop')
  
  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    geom_segment(aes(x = 0,
                     y = user_power,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") + 
    # Color for non-user entered
    # geom_point(power_data_non_user %>% filter(N %% 5 == 0),mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user, mapping = aes(x = N, y = power, group = alpha_level), color = "#999999") +
    # Color for user entered
    # geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = user_power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N, y = max_power + 0.01, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N), y = max(power_data_user$power) + 0.01, label = alpha), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample T-test by alpha level",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified power and resulting sample size.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(ylim = c(0.45, 1))
}

# Z test alpha stratified power curve
Pow_alpha_pow_curve_z <- function(N, effect_size, alpha, user_power, side)
{
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets){
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, 0.99), 0.01), 3)) %>%
      arrange(power) %>%
      mutate(alpha_level = rep.int(alpha_presets, length(power)/3),
             N = map2_dbl(power, alpha_level, ~ceiling(pwr.norm.test(power = .x, sig.level = .y, d = effect_size, alternative = side)[["n"]])))
  }else{
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, user_power), 0.01), 4)) %>%
      arrange(power) %>%
      mutate(alpha_level = rep.int(c(0.01, 0.05, 0.10, alpha), length(power)/4),
             N = map2_dbl(power, alpha_level, ~ceiling(pwr.norm.test(power = .x, sig.level = .y, d = effect_size, alternative = side)[["n"]])))
  }

  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)

  power_data_user <- power_data %>%
    filter(alpha == alpha_level)

  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max_power = max(power),
              min_power = min(power),
              max_N = max(N),
              .groups = "drop")

  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    geom_segment(aes(x = 0,
                     y = user_power,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    # Color for non-user entered
    # geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user %>% filter, mapping = aes(x = N, y = power, group = alpha_level), color = "#999999") +
    # Color for user entered
    # geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = user_power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N, y = max_power + 0.01, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N), y = max(power_data_user$power) + 0.01, label = alpha), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample Z-test by alpha level",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified power and resulting sample size.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(ylim = c(0.45, 1))
}

# T test effect size stratified power curve
Pow_effect_pow_curve_t <- function(N, effect_size, alpha, user_power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, 0.99), 0.01), 3)) %>%
      arrange(power) %>%
      mutate(effect_val = rep.int(std_effect, length(power)/3),
             N = map2_dbl(power, effect_val, ~ceiling(pwr.t.test(power = .x, sig.level = alpha, type = "one.sample", d = .y, alternative = side)[["n"]])))
  }else{
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, user_power), 0.01), 4)) %>%
      arrange(power) %>%
      mutate(effect_val = rep.int(c(0.2, 0.5, 0.8, effect_size), length(power)/4),
             N = map2_dbl(power, effect_val, ~ceiling(pwr.t.test(power = .x, sig.level = alpha, type = "one.sample", d = .y, alternative = side)[["n"]])))
  }

  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)

  power_data_user <- power_data %>%
    filter(effect_size == effect_val)

  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max_power = max(power),
              min_power = min(power),
              max_N = max(N),
              .groups = "drop")

  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    geom_segment(aes(x = 0,
                     y = user_power,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    # Color for non-user entered
    # geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user, mapping = aes(x = N, y = power, group = effect_val), color = "#999999") +
    # Color for user entered
    # geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = user_power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N, y = max_power + 0.01, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N), y = max(power_data_user$power) + 0.01, label = effect_size), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample T-test by effect size",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified power and resulting sample size.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(ylim = c(0.45, 1))
}

# Z test effect size stratified power curve
Pow_effect_pow_curve_z <- function(N, effect_size, alpha, user_power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, 0.99), 0.01), 3)) %>%
      arrange(power) %>%
      mutate(effect_val = rep.int(std_effect, length(power)/3),
             N = map2_dbl(power, effect_val, ~ceiling(pwr.norm.test(power = .x, sig.level = alpha, d = .y, alternative = side)[["n"]])))
  }else{
    power_data <- tibble(power = rep.int(seq(0.50, ifelse(user_power < 0.95, user_power + 0.05, user_power), 0.01), 4)) %>%
      arrange(power) %>%
      mutate(effect_val = rep.int(c(0.2, 0.5, 0.8, effect_size), length(power)/4),
             N = map2_dbl(power, effect_val, ~ceiling(pwr.norm.test(power = .x, sig.level = alpha, d = .y, alternative = side)[["n"]])))
  }

  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)

  power_data_user <- power_data %>%
    filter(effect_size == effect_val)

  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max_power = max(power),
              min_power = min(power),
              max_N = max(N),
              .groups = 'drop')

  ggplot() +
    geom_segment(aes(x = N,
                     y = 0,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    geom_segment(aes(x = 0,
                     y = user_power,
                     xend = N,
                     yend = user_power),
                 alpha = 0.5,
                 color = "#0072B2",
                 lty = "dashed") +
    # Color for non-user entered
    # geom_point(power_data_non_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), color = "#999999", shape = 16) +
    geom_line(power_data_non_user, mapping = aes(x = N, y = power, group = effect_val), color = "#999999") +
    # Color for user entered
    # geom_point(power_data_user %>% filter(N %% 5 == 0), mapping = aes(x = N, y = power), shape = 17, color = "#E69F00") +
    geom_line(power_data_user, mapping = aes(x = N, y = power), color = "#E69F00") +
    geom_point(mapping = aes(x = N, y = user_power), color = "#0072B2", shape = 17, size = 3.25) +
    geom_text(label_non_user, mapping = aes(x = max_N, y = max_power + 0.01, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = max(power_data_user$N), y = max(power_data_user$power) + 0.01, label = effect_size), color = "#E69F00") +
    theme_bw(base_size = 18) +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample Z-test by effect size",
         caption = "Orange power curve based on user specified parameters, with blue triangle indicating the intersection of specified power and resulting sample size.") +
    theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
    coord_cartesian(ylim = c(0.45, 1))
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
# sd_known <- FALSE
# N <- 20
# power <- 0.80
# alpha <- 0.07
# effect_size <- abs(mu0 - muA)/sd
# alternative <- c("greater", "two.sided")
# pow_curve <- 2
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
#   if(pow_curve == 2)
#   {
#     # Alpha case
#     if(sd_known == FALSE){
#       # Student's T
#       N_alpha_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#     } else{
#       # Normal
#       N_alpha_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#     }
#   } else{
#     # Effect size case
#     if(sd_known == FALSE){
#       # Student's T
#       N_effect_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#     } else{
#       # Normal
#       N_effect_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#     }
#   }
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
#   if(pow_curve == 2)
#   {
#     # Alpha case
#     if(sd_known == FALSE){
#       # Student's T
#       Pow_alpha_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#     } else{
#       # Normal
#       Pow_alpha_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#     }
#   } else{
#     # Effect size case
#     if(sd_known == FALSE){
#       # Student's T
#       Pow_effect_pow_curve_t(N, effect_size, alpha, power, alternative[two_sided])
#     } else{
#       # Normal
#       Pow_effect_pow_curve_z(N, effect_size, alpha, power, alternative[two_sided])
#     }
#   }
# }
