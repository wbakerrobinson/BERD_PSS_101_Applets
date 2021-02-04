# One-Sample T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar
# Flexible version to allow for calculation of most values
# This file stores the ggplot functions required for the traditional power viz

# Functions that return ggplot obj for output$power_viz
# 6 functions for the 6 cases. 4 fx for normal and 2 fx students t to handle the one vs two sided and mu0 greater or less than muA
# Student's T
two_sided_t <- function(data, test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  plot_size <- 18
  text_size <- 5
  
  ggplot(data) + 
    geom_area(mapping = aes(y = ifelse(x_val < -test_stat | x_val > test_stat, pdf_h0, 0),
                            x= x_val, 
                            fill = "Type I Error (alpha)")) +
    geom_area(mapping = aes(y = ifelse(x_val < test_stat, pdf_hA, 0), 
                            x= x_val, 
                            fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val <= test_stat & x_val >= -test_stat), 
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0), 
                              ymin = pdf_hA, 
                              x = x_val, 
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val >= test_stat), 
                mapping = aes(ymin = pdf_h0, 
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA), 
                              x = x_val, 
                              fill = "Power")) +
    geom_ribbon(data %>% filter(x_val <= -test_stat), 
                mapping = aes(ymin = 0, 
                              ymax = pdf_hA, 
                              x = x_val, 
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.001 & x_val > 0)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = -test_stat, 
                     y = 0, 
                     xend = -test_stat, 
                     yend = max_h0),
                 alpha = 0.7,
                 color = "#999999") + 
    geom_segment(aes(x = test_stat, 
                     y = 0, xend = test_stat, 
                     yend = max_h0), 
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "T values",
         y = "",
         title = "Traditional Power Visualization: 1 Sample T-test") +
    annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0"), size = text_size) +
    annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA"), size = text_size) +
    annotate("text", x = -test_stat, y = max_h0 + 0.01, label = glue("-Tcrit"), size = text_size) +
    annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("+Tcrit"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

one_sided_t <- function(data, test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  text_size <- 5
  plot_size <- 18
  
  ggplot(data) + 
    geom_area(mapping = aes(y = ifelse(x_val > test_stat, pdf_h0, 0),
                            x= x_val, 
                            fill = "Type I Error (alpha)")) +
    geom_area(mapping = aes(y = ifelse(x_val < test_stat, pdf_hA, 0), 
                            x= x_val, 
                            fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val <= test_stat), 
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0), 
                              ymin = pdf_hA, 
                              x = x_val, 
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val >= test_stat), 
                mapping = aes(ymin = pdf_h0, 
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA), 
                              x = x_val, 
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.001 & x_val > 0)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = test_stat, 
                     y = 0, xend = test_stat, 
                     yend = max_h0), 
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "T values",
         title = "Traditional Power Visualization: 1 Sample T-test") +
    annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0"), size = text_size) +
    annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA"), size = text_size) +
    annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("Tcrit"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

# Normal
two_sided_z_h0_leq <- function(data, mu0, muA, test_stat, neg_test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  label_height <- ifelse(max_h0 < 0.227, max_h0 + 0.023, 0.25)
  plot_size <- 18
  text_size <- 5
  
  ggplot(data) + 
    geom_ribbon(data %>% filter(x_val < neg_test_stat),
                mapping = aes(ymax = pdf_h0,
                              ymin = pdf_hA,
                              x= x_val,
                              fill = "Type I Error (alpha)")) +
    geom_ribbon(data %>% filter(x_val > test_stat),
                mapping = aes(ymax = pdf_h0,
                              ymin = 0,
                              x= x_val,
                              fill = "Type I Error (alpha)")) +
    geom_ribbon(data %>% filter(x_val < test_stat),
                mapping = aes(ymax = pdf_hA,
                              ymin = 0,
                              x= x_val,
                              fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val <= test_stat & x_val >= neg_test_stat), 
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0), 
                              ymin = pdf_hA, 
                              x = x_val, 
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val >= test_stat), 
                mapping = aes(ymin = pdf_h0, 
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA), 
                              x = x_val, 
                              fill = "Power")) +
    geom_ribbon(data %>% filter(x_val <= neg_test_stat), 
                mapping = aes(ymin = 0, 
                              ymax = pdf_hA, 
                              x = x_val, 
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.001 & x_val > 0)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = neg_test_stat, 
                     y = 0, 
                     xend = neg_test_stat, 
                     yend = label_height - 0.007),
                 alpha = 0.7,
                 color = "#999999") + 
    geom_segment(aes(x = test_stat, 
                     y = 0,
                     xend = test_stat, 
                     yend = label_height - 0.007), 
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "Mean Values",
         y = "",
         title = "Traditional Power Visualization: 1 Sample Z-test") +
    annotate("text", x = mu0, y = label_height - 0.016, label = glue("H0"), size = text_size) +
    annotate("text", x = muA, y = label_height - 0.016, label = glue("HA"), size = text_size) +
    annotate("text", x = neg_test_stat, y = label_height, label = glue("-Critical Value"), size = text_size) +
    annotate("text", x = test_stat, y = label_height, label = glue("+Critical Value"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

one_sided_z_h0_leq <- function(data, mu0, muA, test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  label_height <- ifelse(max_h0 < 0.227, max_h0 + 0.023, 0.25)
  text_size <- 5
  plot_size <- 18
  
  ggplot(data) + 
    geom_area(mapping = aes(y = ifelse(x_val > test_stat, pdf_h0, 0),
                            x= x_val, 
                            fill = "Type I Error (alpha)")) +
    geom_area(mapping = aes(y = ifelse(x_val < test_stat, pdf_hA, 0), 
                            x= x_val, 
                            fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val <= test_stat), 
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0), 
                              ymin = pdf_hA, 
                              x = x_val, 
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val >= test_stat), 
                mapping = aes(ymin = pdf_h0, 
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA), 
                              x = x_val, 
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.001 & x_val > 0)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = test_stat, 
                     y = 0,
                     xend = test_stat, 
                     yend = label_height - 0.007), 
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "Mean Values",
         y = "",
         title = "Traditional Power Visualization: 1 Sample Z-test") +
    annotate("text", x = mu0, y = label_height - 0.016, label = glue("H0"), size = text_size) +
    annotate("text", x = muA, y = label_height - 0.016, label = glue("HA"), size = text_size) +
    annotate("text", x = test_stat, y = label_height, label = glue("Critical Value"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

two_sided_z_h0_greater <- function(data, mu0, muA, test_stat, neg_test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  label_height <- ifelse(max_h0 < 0.227, max_h0 + 0.023, 0.25)
  plot_size <- 18
  text_size <- 5
  
  ggplot(data) + 
    geom_ribbon(data %>% filter(x_val < neg_test_stat),
                mapping = aes(ymax = pdf_h0,
                              ymin = 0,
                              x= x_val,
                              fill = "Type I Error (alpha)")) +
    geom_ribbon(data %>% filter(x_val > test_stat),
                mapping = aes(ymax = pdf_h0,
                              ymin = 0,
                              x= x_val,
                              fill = "Type I Error (alpha)")) +
    geom_ribbon(data %>% filter(x_val > neg_test_stat),
                mapping = aes(ymax = pdf_hA,
                              ymin = 0,
                              x= x_val,
                              fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val <= test_stat & x_val >= neg_test_stat),
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0),
                              ymin = pdf_hA,
                              x = x_val,
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val <= neg_test_stat),
                mapping = aes(ymin = pdf_h0,
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA),
                              x = x_val,
                              fill = "Power")) +
    geom_ribbon(data %>% filter(x_val >= test_stat),
                mapping = aes(ymin = 0,
                              ymax = pdf_hA,
                              x = x_val,
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.001)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = neg_test_stat, 
                     y = 0, 
                     xend = neg_test_stat, 
                     yend = label_height - 0.007),
                 alpha = 0.7,
                 color = "#999999") + 
    geom_segment(aes(x = test_stat, 
                     y = 0,
                     xend = test_stat, 
                     yend = label_height - 0.007), 
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "Mean Values",
         y = "",
         title = "Traditional Power Visualization: 1 Sample Z-test") +
    annotate("text", x = mu0, y = label_height - 0.016, label = glue("H0"), size = text_size) +
    annotate("text", x = muA, y = label_height - 0.016, label = glue("HA"), size = text_size) +
    annotate("text", x = neg_test_stat, y = label_height, label = glue("-Critical Value"), size = text_size) +
    annotate("text", x = test_stat, y = label_height, label = glue("+Critical Value"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

one_sided_z_h0_greater <- function(data, mu0, muA, neg_test_stat, color)
{
  max_h0 <- max(data$pdf_h0)
  label_height <- ifelse(max_h0 < 0.227, max_h0 + 0.023, 0.25)
  plot_size <- 18
  text_size <- 5
  
  ggplot(data) + 
    geom_ribbon(data %>% filter(x_val < neg_test_stat),
                mapping = aes(ymax = pdf_h0,
                              ymin = 0,
                              x= x_val,
                              fill = "Type I Error (alpha)")) +
    geom_ribbon(data %>% filter(x_val > neg_test_stat),
                mapping = aes(ymax = pdf_hA,
                              ymin = 0,
                              x= x_val,
                              fill = "Type II Error (Beta)")) +
    geom_ribbon(data %>% filter(x_val >= neg_test_stat),
                mapping = aes(ymax = ifelse(pdf_h0 < pdf_hA, pdf_hA, pdf_h0),
                              ymin = pdf_hA,
                              x = x_val,
                              fill = "True Negative")) +
    geom_ribbon(data %>% filter(x_val <= neg_test_stat),
                mapping = aes(ymin = pdf_h0,
                              ymax = ifelse(pdf_hA < pdf_h0, pdf_h0, pdf_hA),
                              x = x_val,
                              fill = "Power")) +
    geom_line(data %>% filter(!(pdf_h0 < 0.007)),
              mapping = aes(x = x_val,
                            y = pdf_h0),
              color = "#999999",
              lty = "dashed") +
    geom_segment(aes(x = neg_test_stat, 
                     y = 0, 
                     xend = neg_test_stat, 
                     yend = label_height - 0.007),
                 alpha = 0.7,
                 color = "#999999") + 
    labs(x = "Mean Values",
         y = "",
         title = "Traditional Power Visualization: 1 Sample Z-test") +
    annotate("text", x = mu0, y = label_height - 0.016, label = glue("H0"), size = text_size) +
    annotate("text", x = muA, y = label_height - 0.016, label = glue("HA"), size = text_size) +
    annotate("text", x = neg_test_stat, y = label_height, label = glue("Critical Value"), size = text_size) +
    theme_bw(base_size = plot_size) +
    scale_fill_manual(name = "Area Represents:", values = color) +
    theme(legend.position="bottom")
}

# ---------------------------------------- CODE BELOW IS FOR TESTING PURPOSES--------------------------------------------
# color <- c("Type I Error (alpha)" = "#E69F00",
#            "Type II Error (Beta)" = "#F0E442",
#            "Power" = "#0072B2",
#            "True Negative" = "#CCCCCC")
# 
# power <- -1
# N <- -1
# mu0 <- 0
# muA <- 1
# sd <- 1
# two_sided <- 2
# effect_size <- abs(mu0 - muA)/sd
# which_calc <- 1
# sd_known <- FALSE
# N <- 20
# alpha <- 0.05
# alternative <- c("greater", "two.sided")
# if(which_calc == 1){
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
# if(sd_known == FALSE)
# {
#   # T distribution
#   df <- N - 1
#   test_stat <- qt(1 - alpha / two_sided, df = df)
#   ncp <- abs(muA - mu0) * sqrt(N)/sd
#   data <- tibble(x_val = seq(from = -3.5*(df/(df-2)),
#                              to = ncp + ifelse(ncp > 4, ncp + 1, 3.5)*(df/(df-2)),
#                              by = 0.01),
#                  pdf_h0 = dt(x_val, df),
#                  pdf_hA = dt(x_val, df, ncp)) %>%
#     filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > ncp & pdf_hA < 0.001))
#   if(two_sided == 2) {
#     two_sided_t(data, test_stat, color)
#   } else{
#     one_sided_t(data, test_stat, color)
#   }
# } else{
#   # Z distribution
#   # Values not unique to below if else
#   distr_sd <- sd/sqrt(N)
#   test_stat <- qnorm(1 - alpha/two_sided, mean = mu0, sd = distr_sd)
#   neg_test_stat <- ifelse(mu0 != 0, qnorm(alpha/two_sided, mean = mu0, sd = distr_sd), -test_stat)
# 
#   # mu0 <= muA
#   if(mu0 <= muA){
#     upr_h0 <- mu0 + 3.5*distr_sd
#     lwr_bound <- mu0 - 3.5*distr_sd
#     upr_bound <- muA + 3.5*distr_sd
#     seq_by <- ifelse(upr_h0 - lwr_bound < 2, 0.001, 0.01)
#     data <- tibble(x_val = seq(from = lwr_bound,
#                                to = upr_bound,
#                                by = seq_by),
#                    pdf_h0 = dnorm(x_val, mean = mu0, sd = distr_sd),
#                    pdf_hA = dnorm(x_val, mean = muA, sd = distr_sd))
#     if(two_sided == 2){
#       two_sided_z_h0_leq(data, mu0, muA, test_stat, neg_test_stat, color)
#     } else{
#       one_sided_z_h0_leq(data, mu0, muA, test_stat, color)
#     }
#   } else {
#     # mu0 > muA
#     upr_hA <- muA + 3.5*distr_sd
#     lwr_bound <- muA - 3.5*distr_sd
#     upr_bound <- mu0 + 3.5*distr_sd
#     seq_by <- ifelse(upr_hA - lwr_bound < 2, 0.001, 0.01)
#     data <- tibble(x_val = seq(from = lwr_bound,
#                                to = upr_bound,
#                                by = seq_by),
#                    pdf_h0 = dnorm(x_val, mean = mu0, sd = distr_sd),
#                    pdf_hA = dnorm(x_val, mean = muA, sd = distr_sd))
#     if(two_sided == 2){
#       two_sided_z_h0_greater(data, mu0, muA, test_stat, neg_test_stat, color)
#     } else{
#       one_sided_z_h0_greater(data, mu0, muA, neg_test_stat, color)
#     }
#   }
# }