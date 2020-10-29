# One-Sample T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pwr)
library(glue)

ui <- fluidPage(

    # Title
    titlePanel("One Sample T-test Power and Sample Size Applet"),
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),

    # Sidebar with drop down for conditional params based on what to calc
    sidebarPanel(fluid = TRUE,
                 width = 3,
                 selectInput("which_calc",
                             h3("Select one to calculate:"),
                             choices = list("Power" = 1,
                                            "Sample Size" = 2)),
                                            #"Minimum Difference" = 3)),
    # Conditional panel for power
    conditionalPanel(condition = "input.which_calc == 1",
                     radioButtons("two_sided",
                                  "Hypothesis:",
                                  choices = list("Two-sided" = 2,
                                                 "One-sided" = 1),
                                  inline = TRUE,
                                  selected = 2),
                     numericInput("mu0",
                                  "Mean under H0:",
                                  value = 0),
                     numericInput("muA",
                                  "Mean under HA:",
                                  value = 1),
                     numericInput("sd",
                                  "Standard Deviation:",
                                  value = 1,
                                  min = 0),
                     checkboxInput("sd_known",
                                  "Standard Deviation Known?",
                                  value = FALSE),
                     sliderInput("N",
                                 "Sample Size:",
                                 min = 1,
                                 max = 150,
                                 value = 20,
                                 step = 1),
                     sliderInput("alpha",
                                 "Alpha:",
                                 min = 0.01,
                                 max = 0.30,
                                 value = 0.05,
                                 step = 0.01),
                     hr(),
                     radioButtons("pow_curve",
                                  "Select to show on Power curve:",
                                  choices = list("Alpha" = 2,
                                                 "Effect Size" = 1),
                                  inline = TRUE,
                                  selected = 2)),
        # Conditional panel for sample size
        conditionalPanel(condition = "input.which_calc == 2",
                        radioButtons("two_sided_N",
                                     "Hypothesis:",
                                     choices = list("Two-sided" = 2,
                                                    "One-sided" = 1),
                                     inline = TRUE,
                                     selected = 2),
                        numericInput("mu0_N",
                                     "Mean under H0:",
                                     value = 0),
                        numericInput("muA_N",
                                     "Mean under HA:",
                                     value = 1),
                        numericInput("sd_N",
                                     "Standard Deviation:",
                                     value = 1,
                                     min = 0),
                        checkboxInput("sd_known_N",
                                     "Standard Deviation Known?",
                                     value = FALSE),
                        sliderInput("power_N",
                                    "Power:",
                                    min = 0.50,
                                    max = 0.99,
                                    value = 0.80,
                                    step = 0.01),
                        sliderInput("alpha_N",
                                    "Alpha:",
                                    min = 0.01,
                                    max = 0.30,
                                    value = 0.05,
                                    step = 0.01),
                        hr(),
                        radioButtons("pow_curve",
                                     "Select to show on Power curve:",
                                     choices = list("Alpha" = 2,
                                                    "Effect Size" = 1),
                                     inline = TRUE,
                                     selected = 2))),
        # # Conditional panel for mean dif if add to app
        # conditionalPanel(condition = "input.which_calc == 3"),

        # Plots and maybe power etc. Want to tab set?
        mainPanel(
            h3(htmlOutput("header")),
            tabsetPanel(type = "tabs",
                        tabPanel("Power Visualization", plotOutput("power_viz")),
                        tabPanel("Power Curve"),
                        selected = "Power Visualization")
        )
)

server <- function(input, output) {
    # used in determining side of pwr.func
    alternative <- c("greater", "two.sided")
    
    output$header <- renderUI({
        crit_output <- ''
        return_HTML <- ''
        if(as.numeric(input$which_calc) == 1) {
            # use power conditional panel params and calc power
            two_sided <- as.numeric(input$two_sided)
            user_effect <- abs(input$mu0-input$muA)/input$sd
            
            distribution <- paste('Distribution:', ifelse(input$sd_known == FALSE, 'Student\'s t', 'Normal'))
            
            N <- input$N
            N_output <- paste("Sample Size:", ceiling(N))
            
            if(input$sd_known == FALSE)
            {
                crit_val <- qt(1 - input$alpha / two_sided, df = N-1)
                crit_output <- sprintf("T Critival Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 5))
            }else{
                crit_val <- qnorm(1 - input$alpha/two_sided, mean = input$mu0, sd = input$sd/sqrt(N))
                crit_output <- sprintf("Z Critical Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 5))
            }
            
            effect_size <- paste("Effect Size:", user_effect)
            
            power <- ifelse(input$sd_known == FALSE,
                            pwr.t.test(n = input$N,
                                       d = user_effect,
                                       sig.level = input$alpha,
                                       alternative = alternative[[two_sided]])[["power"]],
                            pwr.norm.test(n = input$N,
                                          d = user_effect,
                                          sig.level = input$alpha,
                                          alternative = alternative[[two_sided]])[["power"]])
            
            power_output <- paste("Power:", round(power, 5))
            
            return_HTML <- HTML(paste(distribution, crit_output, effect_size, power_output, N_output, sep = '<br/>'))
            
        } else {
           # use N conditional panel params and sample size
            two_sided <- as.numeric(input$two_sided_N)
            user_effect <- abs(input$mu0_N - input$muA_N)/input$sd_N
            
            distribution <- paste('Distribution:', ifelse(input$sd_known_N == FALSE, 'Student\'s t', 'Normal'))
            
            N <- ifelse(input$sd_known_N == FALSE,
                        pwr.t.test(d = user_effect,
                                   sig.level = input$alpha_N,
                                   power = input$power_N,
                                   alternative = alternative[[two_sided]])[["n"]],
                        pwr.norm.test(d = user_effect,
                                      sig.level = input$alpha_N,
                                      power = input$power_N,
                                      alternative = alternative[[two_sided]])[["n"]])
            N_output <- paste("Sample Size:", ceiling(N))
            
            if(input$sd_known_N == FALSE)
            {
                crit_val <- qt(1 - input$alpha_N / two_sided, df = N-1)
                crit_output <- sprintf("T Critival Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 5))
            }else{
                crit_val <- qnorm(1 - input$alpha_N/two_sided, mean = input$mu0_N, sd = input$sd_N/sqrt(N))
                crit_output <- sprintf("Z Critical Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 5))
            }
            
            effect_size <- paste("Effect Size:", user_effect)
            
            power <- input$power_N
            power_output <- paste("Power:", round(power, 5))
            
            return_HTML <- HTML(paste(distribution, crit_output, effect_size, power_output, N_output, sep = '<br/>'))
        }
       return(return_HTML) 
    })
    
    output$power_viz <- renderPlot({
        # All plots have same palette 
        cols <- c("Type I Error (alpha)" = "#E69F00", 
                  "Type II Error (Beta)" = "#F0E442", 
                  "Power" = "#0072B2", 
                  "True Negative" = "#CCCCCC")
        if(as.numeric(input$which_calc) == 1){
            # Params power conditional panel
            alpha <- input$alpha
            mu0 <- input$mu0 
            muA <- input$muA 
            sd <- input$sd 
            effect_size <- abs(mu0 - muA)/sd
            N <- input$N
            df <- N-1
            two_sided <- as.numeric(input$two_sided)
            
           if(input$sd_known == FALSE){
               test_stat <- qt(1 - alpha / two_sided, df = N-1)
               ncp <- (muA - mu0) * sqrt(N)/sd
               data <- tibble(x_val = seq(from = -3.5*(df/(df-2)), 
                                          to = ncp + ifelse(ncp > 4, ncp + 1, 3.5)*(df/(df-2)), 
                                          by = 0.01),
                              pdf_h0 = dt(x_val, df),
                              pdf_hA = dt(x_val, df, ncp)) %>%
                   filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > ncp & pdf_hA < 0.001))
               max_h0 <- max(data$pdf_h0)
               
               if(two_sided == 2){
               # 2 sided T distr power plot
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
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample T-test") +
                       annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0")) +
                       annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA")) +
                       annotate("text", x = -test_stat, y = max_h0 + 0.01, label = glue("-Tcrit")) +
                       annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("+Tcrit")) +
                       theme_bw() +
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
                   
               } else{
               # 1 sided T distr power plot
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
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample T-test") +
                       annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0")) +
                       annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA")) +
                       annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("Tcrit")) +
                       theme_bw() + # will create my own theme eventually
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
               }
           } else{ 
               test_stat <- qnorm(1 - alpha/two_sided, mean = mu0, sd = sd/sqrt(N))
               se <- sd/sqrt(N)
               data <- tibble(x_val = seq(from = mu0 -3.5*se, 
                                          to = muA + 3.5*se, 
                                          by = 0.01),
                              pdf_h0 = dnorm(x_val, mean = mu0, sd = se),
                              pdf_hA = dnorm(x_val, mean = muA, sd = se)) %>%
                   filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > muA & pdf_hA < 0.001))
               max_h0 <- max(data$pdf_h0)
               label_height <- max_h0 + max_h0/50
               
               if(two_sided == 2){
                   # 2 sided norm distr power plot
                   ggplot(data) + 
                       geom_ribbon(data %>% filter(x_val < -test_stat),
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
                                        y = 0,
                                        xend = test_stat, 
                                        yend = max_h0), 
                                    alpha = 0.7,
                                    color = "#999999") + 
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample Z-test") +
                       annotate("text", x = mu0, y = label_height, label = glue("H0")) +
                       annotate("text", x = muA, y = label_height, label = glue("HA")) +
                       annotate("text", x = -test_stat, y = label_height, label = glue("-Tcrit")) +
                       annotate("text", x = test_stat, y = label_height, label = glue("+Tcrit")) +
                       theme_bw() + # will create my own theme eventually
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
                   
               } else{
                   # 1 sided norm distr power plot
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
                                        yend = max_h0), 
                                    alpha = 0.7,
                                    color = "#999999") + 
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample Z-test") +
                       annotate("text", x = mu0, y = label_height, label = glue("H0")) +
                       annotate("text", x = muA, y = label_height, label = glue("HA")) +
                       annotate("text", x = test_stat, y = label_height, label = glue("Tcrit")) +
                       theme_bw() +
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
               }
           }
        } else{
            # Params N conditional panel
            alpha <- input$alpha_N
            mu0 <- input$mu0_N
            muA <- input$muA_N
            sd <- input$sd_N
            effect_size <- abs(mu0 - muA)/sd
            power <- input$power_N
            two_sided <- as.numeric(input$two_sided_N)
            
           if(input$sd_known_N == FALSE){
               # T distribution traditional plots
               N <- pwr.t.test(d = effect_size,
                          sig.level = input$alpha_N,
                          power = input$power_N,
                          alternative = alternative[[two_sided]])[["n"]]
               df <- N-1
               test_stat <- qt(1 - alpha / two_sided, df = df)
               ncp <- (muA - mu0) * sqrt(N)/sd
               data <- tibble(x_val = seq(from = -3.5*(df/(df-2)), 
                                          to = ncp + ifelse(ncp > 4, ncp + 1, 3.5)*(df/(df-2)), 
                                          by = 0.01),
                              pdf_h0 = dt(x_val, df),
                              pdf_hA = dt(x_val, df, ncp)) %>%
                   filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > ncp & pdf_hA < 0.001))
               max_h0 <- max(data$pdf_h0)
               label_height <- max_h0 + max_h0/50
               
               if(two_sided == 2){
                   # Two-sided T distr power plot
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
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample T-test") +
                       annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0")) +
                       annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA")) +
                       annotate("text", x = -test_stat, y = max_h0 + 0.01, label = glue("-Tcrit")) +
                       annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("+Tcrit")) +
                       theme_bw() +
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
                   
               } else{
                   # One-sided T distr power plot
                   ggplot(data) + 
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
                       labs(y = "Value of Probability Density Function",
                            x = "Value of T statistic",
                            title = "Traditional Power Visualization: 1 Sample Z-test") +
                       annotate("text", x = 0, y = max(data$pdf_h0) + 0.01, label = glue("H0")) +
                       annotate("text", x = data[[which.max(data$pdf_hA),1]], y = max(data$pdf_hA) + 0.01, label = glue("HA")) +
                       annotate("text", x = test_stat, y = max_h0 + 0.01, label = glue("Tcrit")) +
                       theme_bw() +
                       scale_fill_manual(name = "Area Represents:", values = cols) +
                       theme(legend.position="bottom")
               }
           } else{
               N <- pwr.norm.test(d = effect_size,
                                 sig.level = alpha,
                                 power = power,
                                 alternative = alternative[[two_sided]])[["n"]]
               test_stat <- qnorm(1 - alpha/two_sided, mean = mu0, sd = sd/sqrt(N))
               se <- sd/sqrt(N)
               data <- tibble(x_val = seq(from = mu0 - 3.5*se, 
                                          to = muA + 3.5*se, 
                                          by = 0.01),
                              pdf_h0 = dnorm(x_val, mean = mu0, sd = se),
                              pdf_hA = dnorm(x_val, mean = muA, sd = se)) %>%
                   filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > muA & pdf_hA < 0.001))
               max_h0 <- max(data$pdf_h0)
               label_height <- max_h0 + max_h0/50
              
              if(two_sided == 2) {
                  # Two-sided 1 Sample Z test viz
                  ggplot(data) + 
                      geom_ribbon(data %>% filter(x_val < -test_stat),
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
                                       y = 0,
                                       xend = test_stat, 
                                       yend = max_h0), 
                                   alpha = 0.7,
                                   color = "#999999") + 
                      labs(y = "Value of Probability Density Function",
                           x = "Value of T statistic",
                           title = "Traditional Power Visualization: 1 Sample Z-test") +
                      annotate("text", x = mu0, y = label_height, label = glue("H0")) +
                      annotate("text", x = muA, y = label_height, label = glue("HA")) +
                      annotate("text", x = -test_stat, y = label_height, label = glue("-Tcrit")) +
                      annotate("text", x = test_stat, y = label_height, label = glue("+Tcrit")) +
                      theme_bw() + # will create my own theme eventually
                      scale_fill_manual(name = "Area Represents:", values = cols) +
                      theme(legend.position="bottom")
                  
              } else{
                  # One-sided 1 Sample Z test viz
                  ggplot(data) + 
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
                                       yend = max_h0), 
                                   alpha = 0.7,
                                   color = "#999999") + 
                      labs(y = "Value of Probability Density Function",
                           x = "Value of T statistic",
                           title = "Traditional Power Visualization: 1 Sample Z-test") +
                      annotate("text", x = mu0, y = label_height, label = glue("H0")) +
                      annotate("text", x = muA, y = label_height, label = glue("HA")) +
                      annotate("text", x = test_stat, y = label_height, label = glue("Tcrit")) +
                      theme_bw() +
                      scale_fill_manual(name = "Area Represents:", values = cols) +
                      theme(legend.position="bottom")
              }
           }
        }
    })
    # ouput$power_curve <- renderPlot({
    #     
    # })
    
}

shinyApp(ui = ui, server = server)
