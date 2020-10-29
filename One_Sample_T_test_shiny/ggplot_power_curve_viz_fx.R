# One-Sample T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar
# Simplified Version meant for teaching demos
# This file stores the ggplot functions required for the power curve visualization

# T test alpha stratified power curve
alpha_pow_curve_t <- function(N, effect_size, alpha, power, side)
{
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets){
    power_data <- tibble(N = seq(2, N + 5, 1),
                         alpha0.01 = pwr.t.test(n = N, d= effect_size, sig.level = 0.01, alternative = side)[["power"]],
                         alpha0.05 = pwr.t.test(n = N, d = effect_size, sig.level = 0.05, alternative = side)[["power"]],
                         alpha0.10 = pwr.t.test(n = N, d = effect_size, sig.level = 0.10, alternative = side)[["power"]])
  }else{
    power_data <- tibble(N = seq(2, N + 5, 1),
                         alpha0.01 = pwr.t.test(n = N, d = effect_size, sig.level = 0.01, alternative = side)[["power"]],
                         alpha0.05 = pwr.t.test(n = N, d = effect_size, sig.level = 0.05, alternative = side)[["power"]],
                         alpha0.10 = pwr.t.test(n = N, d = effect_size, sig.level = 0.10, alternative = side)[["power"]],
                         user_alpha = pwr.t.test(n = N, d = effect_size, sig.level = alpha, alternative = side)[["power"]]) %>%
      rename(!!paste0("alpha", alpha) := user_alpha)
  }
  power_data <- power_data %>%
    pivot_longer(starts_with("alpha"),
                 names_prefix = "alpha",
                 values_to = "power",
                 names_to = "alpha_level")
  
  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)
  
  power_data_user <- power_data %>%
    filter(alpha == alpha_level)
  
  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max = max(power),
              min = min(power),
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
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 2) +
    geom_text(label_non_user, mapping = aes(x = N + 6, y = max, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = N + 6, y = max(power_data_user$power), label = alpha), color = "#E69F00") +
    theme_bw() +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample T-test by alpha level",
         subtitle = "User specified alpha in orange with (N, power) in blue") +
    theme(legend.position="none")
}

# Z test alpha stratified power curve
alpha_pow_curve_z <- function(N, effect_size, alpha, power, side)
{
  alpha_presets <- c(0.01, 0.05, 0.10)
  if(alpha %in% alpha_presets)
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         alpha0.01 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.01, alternative = side)[["power"]],
                         alpha0.05 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.05, alternative = side)[["power"]],
                         alpha0.10 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.10, alternative = side)[["power"]])
  } else
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         alpha0.01 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.01, alternative = side)[["power"]],
                         alpha0.05 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.05, alternative = side)[["power"]],
                         alpha0.10 = pwr.norm.test(n = N, d = effect_size, sig.level = 0.10, alternative = side)[["power"]],
                         user_alpha = pwr.norm.test(n = N, d = effect_size, sig.level = alpha, alternative = side)[["power"]]) %>%
      rename(!!paste0("alpha", alpha) := user_alpha)
  }
  power_data <- power_data %>%
    pivot_longer(starts_with("alpha"),
                 names_prefix = "alpha",
                 values_to = "power",
                 names_to = "alpha_level")
  
  power_data_non_user <- power_data %>%
    filter(alpha != alpha_level)
  
  power_data_user <- power_data %>%
    filter(alpha == alpha_level)
  
  label_non_user <- power_data_non_user %>%
    group_by(alpha_level) %>%
    summarize(max = max(power),
              min = min(power),
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
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 2) +
    geom_text(label_non_user, mapping = aes(x = N + 6, y = max, label = alpha_level), color = "#999999") +
    geom_text(mapping = aes(x = N + 6, y = max(power_data_user$power), label = alpha), color = "#E69F00") +
    theme_bw() +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample Z-test by alpha level",
         subtitle = "User specified alpha in orange with (N, power) in blue") +
    theme(legend.position="none")
}

# T test effect size stratified power curve
effect_pow_curve_t <- function(N, effect_size, alpha, power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         !!paste0("effect", std_effect[1]) := pwr.t.test(n = N, d = std_effect[1], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[2]) := pwr.t.test(n = N, d = std_effect[2], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[3]) := pwr.t.test(n = N, d = std_effect[3], sig.level = alpha, alternative = side)[["power"]])
  } else
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         !!paste0("effect", std_effect[1]) := pwr.t.test(n = N, d = std_effect[1], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[2]) := pwr.t.test(n = N, d = std_effect[2], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[3]) := pwr.t.test(n = N, d = std_effect[3], sig.level = alpha, alternative = side)[["power"]],
                         user_effect = pwr.t.test(n = N, d = effect_size, sig.level = alpha, alternative = side)[["power"]]) %>%
      rename(!!paste0("effect", effect_size) := user_effect)
  }
  power_data <- power_data %>%
    pivot_longer(starts_with("effect"),
                 names_prefix = "effect",
                 names_transform = list("effect_val" = as.double),
                 values_to = "power",
                 names_to = "effect_val")
  
  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)
  
  power_data_user <- power_data %>%
    filter(effect_size == effect_val)
  
  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max = max(power),
              min = min(power),
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
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 2) +
    geom_text(label_non_user, mapping = aes(x = N + 6, y = max, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = N + 6, y = max(power_data_user$power), label = effect_size), color = "#E69F00") +
    theme_bw() +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample T-test by effect size",
         subtitle = "User specified effect size in orange with (N, power) in blue") +
    theme(legend.position="none")
}

# Z test effect size stratified power curve
effect_pow_curve_z <- function(N, effect_size, alpha, power, side)
{
  std_effect <- c(0.2, 0.5, 0.8)
  if(effect_size %in% std_effect)
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         !!paste0("effect", std_effect[1]) := pwr.norm.test(n = N, d = std_effect[1], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[2]) := pwr.norm.test(n = N, d = std_effect[2], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[3]) := pwr.norm.test(n = N, d = std_effect[3], sig.level = alpha, alternative = side)[["power"]])
  } else
  {
    power_data <- tibble(N = seq(2, N + 5, 1),
                         !!paste0("effect", std_effect[1]) := pwr.norm.test(n = N, d = std_effect[1], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[2]) := pwr.norm.test(n = N, d = std_effect[2], sig.level = alpha, alternative = side)[["power"]],
                         !!paste0("effect", std_effect[3]) := pwr.norm.test(n = N, d = std_effect[3], sig.level = alpha, alternative = side)[["power"]],
                         user_effect = pwr.norm.test(n = N, d = effect_size, sig.level = alpha, alternative = side)[["power"]]) %>%
      rename(!!paste0("effect", effect_size) := user_effect)
  }
  power_data <- power_data %>%
    pivot_longer(starts_with("effect"),
                 names_prefix = "effect",
                 names_transform = list("effect_val" = as.double),
                 values_to = "power",
                 names_to = "effect_val")
  
  power_data_non_user <- power_data %>%
    filter(effect_size != effect_val)
  
  power_data_user <- power_data %>%
    filter(effect_size == effect_val)
  
  label_non_user <- power_data_non_user %>%
    group_by(effect_val) %>%
    summarize(max = max(power),
              min = min(power),
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
    geom_point(mapping = aes(x = N, y = power), color = "#0072B2", shape = 17, size = 2) +
    geom_text(label_non_user, mapping = aes(x = N + 6, y = max, label = effect_val), color = "#999999") +
    geom_text(mapping = aes(x = N + 6, y = max(power_data_user$power), label = effect_size), color = "#E69F00") +
    theme_bw() +
    labs(x = "Sample Size",
         y = "Power",
         title = "Power Curve: 1 sample Z-test by effect size",
         subtitle = "User specified effect size in orange with (N, power) in blue") +
    theme(legend.position="none")
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
