# One-Sample T test app
# Will Baker-Robinson
# BSTA 500 -BERD PSS Seminar
# Simplified Version meant for teaching demos

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pwr)
library(glue)
library(purrr)

# Source files for app
source("ggplot_trad_power_viz_fx.R", local = TRUE)
source("ggplot_N_power_curve_viz_fx.R", local = TRUE)
source("ggplot_Pow_power_curve_viz_fx.R", local = TRUE)

# update line, point, text, segment
update_geom_defaults("line", list(size = 1.15))
update_geom_defaults("segment", list(size = 1.15))
update_geom_defaults("point", list(size = 3))
update_geom_defaults("text", list(size = 5))

ui <- fluidPage(
    
    # Title
    titlePanel("One Sample Z and T-test Power and Sample Size Applet"),
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tags$head(tags$style("#PSS_calc{color: #E69F00;}" 
    )),
    
    # Sidebar with drop down for conditional params based on what to calc
    sidebarPanel(fluid = TRUE,
                 width = 3,
                 selectInput("which_calc",
                             h3("Select one to calculate:"),
                             choices = list("Power" = 1,
                                            "Sample Size" = 2)),
                 radioButtons("two_sided",
                              "Hypothesis:",
                              choices = list("Two-sided" = 2,
                                             "One-sided" = 1),
                              inline = TRUE,
                              selected = 2),
                 sliderInput("mu0",
                              "Mean under H0:",
                             min = 30,
                             max = 50,
                             step = 0.5,
                              value = 34),
                 sliderInput("muA",
                              "Mean under HA:",
                              min = 30,
                              max = 50,
                              value = 46.5,
                              step = 0.5),
                 sliderInput("sd",
                             "Standard Deviation:",
                             min = 10, 
                             max = 30,
                             value = 25.5,
                             step = 0.5),
                 checkboxInput("sd_known",
                               "Standard Deviation Known?",
                               value = TRUE),
                 # Conditional panel for power calc
                 conditionalPanel(condition = "input.which_calc == 1",
                                  sliderInput("N",
                                              "Sample Size:",
                                              min = 5,
                                              max = 50,
                                              value = 35,
                                              step = 1)),
                 # Conditional panel for N calc
                 conditionalPanel(condition = "input.which_calc == 2",
                                  sliderInput("power",
                                              "Power:",
                                              min = 0.60,
                                              max = 0.99,
                                              value = 0.80,
                                              step = 0.01)),
                 sliderInput("alpha",
                             "Alpha:",
                             min = 0.01,
                             max = 0.10,
                             value = 0.05,
                             step = 0.01),
                 conditionalPanel(condition = "input.main_tab == 'Power Curve'",
                                  hr(),
                                  h4(helpText("Power Curve Options")),
                                  radioButtons("pow_curve",
                                               "Stratify Power curve by:",
                                               choices = list("Alpha" = 2,
                                                              "Effect Size" = 1),
                                               inline = TRUE,
                                               selected = 2))),

    mainPanel(
        h3(htmlOutput("header")),
        tabsetPanel(id = "main_tab",
                    type = "tabs",
                    tabPanel("Power Visualization", plotOutput("power_viz", width = "100%")),
                    tabPanel("Power Curve", plotOutput("pow_curve"), width = "100%"),
                    selected = "Power Visualization")
    )
)



server <- function(input, output, session) {
    # used in determining side of pwr.func
    alternative <- c("greater", "two.sided")
    
    # used to change muA slider to a value 1 greater if the user sets the two equal
    observe({
        if(input$muA == input$mu0) {
           if(input$mu0 == 50) {
            updateSliderInput(session,
                              inputId = "muA",
                              value = input$muA - 1)
           }else{
            updateSliderInput(session,
                              inputId = "muA",
                              value = input$muA + 1)
           }
        }
    })
    
    # Values displayed above plot
    output$header <- renderUI({
        # Params
        power <- -1
        N <- -1
        crit_output <- ''
        two_sided <- as.numeric(input$two_sided)
        user_effect <- abs(input$mu0 - input$muA)/input$sd
        # Calc power or sample size based on input
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == FALSE,
                            pwr.t.test(n = input$N,
                                       d = user_effect,
                                       sig.level = input$alpha,
                                       type = "one.sample",
                                       alternative = alternative[[two_sided]])[["power"]],
                            pwr.norm.test(n = input$N,
                                          d = user_effect,
                                          sig.level = input$alpha,
                                          alternative = alternative[[two_sided]])[["power"]])
            N <- input$N
        } else {
            N <- ifelse(input$sd_known == FALSE,
                        pwr.t.test(d = user_effect,
                                   sig.level = input$alpha,
                                   power = input$power,
                                   type = "one.sample",
                                   alternative = alternative[[two_sided]])[["n"]],
                        pwr.norm.test(d = user_effect,
                                      sig.level = input$alpha,
                                      power = input$power,
                                      alternative = alternative[[two_sided]])[["n"]])
            N <- ceiling(N)
            power <- input$power
        }
        # Critical value
        if(input$sd_known == FALSE)
        {
            crit_val <- qt(1 - input$alpha / two_sided, df = N-1)
            crit_output <- sprintf("T Critival Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 3))
        }else{
            if(two_sided == 2){
                # Upper and lower
                crit_val_lwr <- qnorm(input$alpha/two_sided, mean = input$mu0, sd = input$sd/sqrt(N))
                crit_val_upr <- qnorm(1 - input$alpha/two_sided, mean = input$mu0, sd = input$sd/sqrt(N))
                crit_output <- sprintf("Critical Values: %s, %s", round(crit_val_lwr, 3), round(crit_val_upr, 3))
            }
            else{
                # lower or upper
                if(input$mu0 < input$muA)
                {
                    crit_val <- qnorm(1 - input$alpha, mean = input$mu0, sd = input$sd/sqrt(N))
                    crit_output <- sprintf("Critical Value: %s", round(crit_val, 3))
                }
                else
                {
                   crit_val <- qnorm(input$alpha, mean = input$mu0, sd = input$sd/sqrt(N))
                   crit_output <- sprintf("Critical Value: %s", round(crit_val, 3))
                }
            }
        }
        
        # return vars
        distribution <- paste('Distribution:', ifelse(input$sd_known == FALSE, 'Student\'s t', 'Normal'))
        effect_size <- paste("Effect Size:", round(user_effect, 3))
        power_output <- paste("Power:", round(power, 3))
        N_output <- paste("Sample Size:", N)
        
        # Add DF if T distr
        if(input$sd_known == FALSE)
        {
            df <- paste("Degrees of Freedom:", N - 1)
            return_HTML <- HTML(paste(distribution, df, crit_output, effect_size, ifelse(as.numeric(input$which_calc) == 1, N_output, power_output), div(id = "PSS_calc", ifelse(as.numeric(input$which_calc) == 1, power_output, N_output)), sep = '<br/>'))
        }else{
            return_HTML <- HTML(paste(distribution, crit_output, effect_size, ifelse(as.numeric(input$which_calc) == 1, N_output, power_output), div(id = "PSS_calc", ifelse(as.numeric(input$which_calc) == 1, power_output, N_output)), sep = '<br/>'))
        }
        return(return_HTML)
    })

    # Trad power viz
    output$power_viz <- renderPlot({
        # All plots have same palette
        color <- c("Type I Error (alpha)" = "#E69F00",
                  "Type II Error (Beta)" = "#F0E442",
                  "Power" = "#0072B2",
                  "True Negative" = "#CCCCCC")
        power <- -1
        N <- -1
        effect_size <- abs(input$mu0 - input$muA)/input$sd
        two_sided <- as.numeric(input$two_sided)
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == FALSE,
                            pwr.t.test(n = input$N,
                                       d = effect_size,
                                       sig.level = input$alpha,
                                       type = "one.sample",
                                       alternative = alternative[[two_sided]])[["power"]],
                            pwr.norm.test(n = input$N,
                                          d = effect_size,
                                          sig.level = input$alpha,
                                          alternative = alternative[[two_sided]])[["power"]])
            N <- input$N
        } else {
            N <- ifelse(input$sd_known == FALSE,
                        pwr.t.test(d = effect_size,
                                   sig.level = input$alpha,
                                   power = input$power,
                                   type = "one.sample",
                                   alternative = alternative[[two_sided]])[["n"]],
                        pwr.norm.test(d = effect_size,
                                      sig.level = input$alpha,
                                      power = input$power,
                                      alternative = alternative[[two_sided]])[["n"]])
            N <- ceiling(N)
            power <- input$power
        }
        if(input$sd_known == FALSE)
        {
            # T distribution
            df <- N - 1
            test_stat <- qt(1 - input$alpha / two_sided, df = df)
            ncp <- abs(input$muA - input$mu0) * sqrt(N)/input$sd
            data <- tibble(x_val = seq(from = -3.5*(df/(df-2)),
                                       to = ncp + ifelse(ncp > 4, ncp + 1, 3.5)*(df/(df-2)),
                                       by = 0.01),
                           pdf_h0 = dt(x_val, df),
                           pdf_hA = dt(x_val, df, ncp)) %>%
                filter(!(x_val < 0 & pdf_h0 < 0.001) & !(x_val > ncp & pdf_hA < 0.001))
            if(two_sided == 2) {
                two_sided_t(data, test_stat, color)
            } else{
                one_sided_t(data, test_stat, color)
            }
        }
        else{
            # Z distribution
            # Values not unique to below if else
            distr_sd <- input$sd/sqrt(N)
            test_stat <- qnorm(1 - input$alpha/two_sided, mean = input$mu0, sd = distr_sd)
            neg_test_stat <- ifelse(input$mu0 != 0, qnorm(input$alpha/two_sided, mean = input$mu0, sd = distr_sd), -test_stat)

            # mu0 <= muA
            if(input$mu0 <= input$muA){
                upr_h0 <- input$mu0 + 3.5*distr_sd
                lwr_bound <- input$mu0 - 3.5*distr_sd
                upr_bound <- input$muA + 3.5*distr_sd
                seq_by <- ifelse(upr_h0 - lwr_bound < 2, 0.001, 0.01)
                data <- tibble(x_val = seq(from = lwr_bound,
                                           to = upr_bound,
                                           by = seq_by),
                               pdf_h0 = dnorm(x_val, mean = input$mu0, sd = distr_sd),
                               pdf_hA = dnorm(x_val, mean = input$muA, sd = distr_sd))
                if(two_sided == 2){
                    two_sided_z_h0_leq(data, input$mu0, input$muA, test_stat, neg_test_stat, color)
                } else{
                    one_sided_z_h0_leq(data, input$mu0, input$muA, test_stat, color)
                }
            } else {
                # mu0 > muA
                upr_hA <- input$muA + 3.5*distr_sd
                lwr_bound <- input$muA - 3.5*distr_sd
                upr_bound <- input$mu0 + 3.5*distr_sd
                seq_by <- ifelse(upr_hA - lwr_bound < 2, 0.001, 0.01)
                data <- tibble(x_val = seq(from = lwr_bound,
                                           to = upr_bound,
                                           by = seq_by),
                               pdf_h0 = dnorm(x_val, mean = input$mu0, sd = distr_sd),
                               pdf_hA = dnorm(x_val, mean = input$muA, sd = distr_sd))
                if(two_sided == 2){
                    two_sided_z_h0_greater(data, input$mu0, input$muA, test_stat, neg_test_stat, color)
                } else{
                    one_sided_z_h0_greater(data, input$mu0, input$muA, neg_test_stat, color)
                }
            }
        }
    }, height = 555)

    #Power curve
    output$pow_curve <- renderPlot({
        power <- -1
        N <- -1
        effect_size <- round(abs(input$mu0 - input$muA)/input$sd, 3)
        two_sided <- as.numeric(input$two_sided)
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == FALSE,
                            pwr.t.test(n = input$N,
                                       d = effect_size,
                                       sig.level = input$alpha,
                                       type = "one.sample",
                                       alternative = alternative[[two_sided]])[["power"]],
                            pwr.norm.test(n = input$N,
                                          d = effect_size,
                                          sig.level = input$alpha,
                                          alternative = alternative[[two_sided]])[["power"]])
            N <- input$N
        } else {
            N <- ifelse(input$sd_known == FALSE,
                        pwr.t.test(d = effect_size,
                                   sig.level = input$alpha,
                                   power = input$power,
                                   type = "one.sample",
                                   alternative = alternative[[two_sided]])[["n"]],
                        pwr.norm.test(d = effect_size,
                                      sig.level = input$alpha,
                                      power = input$power,
                                      alternative = alternative[[two_sided]])[["n"]])
            N <- ceiling(N)
            power <- input$power
        }
        if(as.numeric(input$pow_curve) == 2)
        {
            # Alpha case
            if(input$sd_known == FALSE){
                # Student's T
                if(as.numeric(input$which_calc) == 1)
                    N_alpha_pow_curve_t(N, effect_size, input$alpha, power, alternative[two_sided])
                else
                    Pow_alpha_pow_curve_t(N, effect_size, input$alpha, power, alternative[two_sided])
            } else{
                # Normal
                if(as.numeric(input$which_calc) == 1)
                    N_alpha_pow_curve_z(N, effect_size, input$alpha, power, alternative[two_sided])
                else
                    Pow_alpha_pow_curve_z(N, effect_size, input$alpha, power, alternative[two_sided])
            }
        } else{
            # Effect size case
            if(input$sd_known == FALSE){
                # Student's T
                if(as.numeric(input$which_calc) == 1)
                    N_effect_pow_curve_t(N, effect_size, input$alpha, power, alternative[two_sided])
                else
                    Pow_effect_pow_curve_t(N, effect_size, input$alpha, power, alternative[two_sided])
            } else{
                # Normal
                if(as.numeric(input$which_calc) == 1)
                    N_effect_pow_curve_z(N, effect_size, input$alpha, power, alternative[two_sided])
                else
                    Pow_effect_pow_curve_z(N, effect_size, input$alpha, power, alternative[two_sided])
            }
        }
    }, height = 600)
}

shinyApp(ui = ui, server = server)
