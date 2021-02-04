# Paired T test app
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
    titlePanel("Paired Z and T-test Power and Sample Size Applet"),
    withMathJax(),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tags$head(tags$style("#PSS_calc{color: #E69F00;}" 
    )),
    tags$head(tags$style("#about_p{font-size: 18px;}")),
    tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']], processEscapes: true}});"),
    
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
                 sliderInput("muA",
                              "Difference HA and H0:",
                              min = -15,
                              max = 15,
                              value = 12.5,
                              step = 0.5),
                 sliderInput("sd",
                             "Standard Deviation:",
                             min = 10, 
                             max = 30,
                             value = 25.5,
                             step = 0.5),
                 radioButtons("sd_known",
                              "Standard Deviation Assumption:",
                              choices = list("Known" = 1,
                                             "Unknown" = 2),
                              inline = TRUE,
                              selected = 1),
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
                    tabPanel("About",
                             width = "100%",
                             tags$br(),
                             div(id = "about_p", p("This app calculates power and sample size for a one-sample Z and T-test. 
        It was designed to give learners an intuitive understanding of these calculations by 
        visualizing the way changes in parameters yield different power curves and distributions.")),
                             p(h4("Calculations"),
                               "Power and sample size was calculated using the R library pwr. Specificially the functions pwr.norm.test, and pwr.t.test
                               were used. Please see the pwr package's",
                               tags$a(href = "https://cran.r-project.org/web/packages/pwr/pwr.pdf", "reference"),
                               "for more details. The source code for this app can also be found",
                               tags$a(href = "https://github.com/wbakerrobinson/PSS_Applets_BERD/tree/main/One_Sample_T_test_shiny", "here."),
                               "There is also a supplementary rmarkdown file (",
                               tags$a(href = "", "here"),
                               ") to go along with the BERD Power and Sample Size 101 Workshop examples,
                               which shows how to perform these calculations using the base stats package in R."),
                             h4("Overview of App Options"),
                             p(h5("Hypothesis:"),
                               "Two-sided: $H_0: \\mu = \\mu_0$ vs. $H_A: \\mu \\ne \\mu_0$",
                               tags$br(),
                               "One-sided: $H_0: \\mu = \\mu_0$ vs. $H_A: \\mu < \\mu_0$",
                               "  or  ",
                               "$H_0: \\mu = \\mu_0$ vs. $H_A: \\mu > \\mu_0$",
                               tags$br(),
                               "For the one-sided hypothesis the app selects the inequality depending on the user
                               specified values of the mean for the null and alternative distribution. That is, if the 
                               mean of $H_A$ is greater than $H_0$, the app selects the alternative hypothesis $\\mu > \\mu_0$."),
                             p(h5("Mean $H_0$ and $H_A$:"), "The population mean under the null hypothesis,
                               and the population mean for which the power and sample size are calculated."),
                             p(h5("Standard Deviation:"), "The best estimate of population standard deviation."),
                             p(h5("Standard Deviation Assumption:"),
                               "Known: This calculates the power or sample size using a normal distribution. This is an unrealistic
                               assumption to make in most real-world situations. It is included here for learning purposes.",
                               tags$br(),
                               "Unknown: This calculates the power or sample size using a Student's T distribution. You may notice that
                               the distribution of HA is not symmetric, this is because it follows the non-central T distribution."),
                             p(h5("Power:"), "This is an option when the user has selected to calculate sample size. Power is the 
                               probability that we reject the null hypothesis, when the null hypothesis is false."),
                             p(h5("Sample Size:"), "This an option when the user has selected to calculate power. 
                               Sample size is the estimated number of participants we will have in our data."),
                             p(h5("Alpha:"), "The type one error rate, or the probability of a false positive. 
                               We choose a small alpha level to ensure a low probability of rejecting the null hypothesis when it is in fact true."),
                             p(h5("Power Curve Options:"),
                               "Alpha: The power curves corresponding to common alpha values of 0.01, 0.05, and 0.10 are shown, as well as the user
                               input value if it is not one of these.",
                               tags$br(),
                               "Effect Size: The power curves corresponding to the effect sizes of 0.2, 0.5, 0.8 are shown, as well as the user input
                               value if it is not one of thse. Effect sizes were chosen based on Cohen's rule of thumb where an effect of 0.2 is considered
                               small, 0.5 is considered a moderate effect, and 0.8 is considered a large effect."),
                             tags$hr(),
                             p("Created by Will Baker-Robinson with feedback from Meike Niederhausen, Yiyi Chen, and Alicia Johnson."),
                             p("Email bakerrob@ohsu.edu with questions or feedback.")
                    ),
                    selected = "Power Visualization")
    )
)



server <- function(input, output, session) {
    # used in determining side of pwr.func
    alternative <- c("greater", "two.sided")
    
    # used to change muA slider to a value 1 greater if the user sets the two equal
    observe({
        mu0 <- 0
        if(input$muA == mu0) {
            updateSliderInput(session,
                              inputId = "muA",
                              value = input$muA + 1)
        }
    })
    
    # Values displayed above plot
    output$header <- renderUI({
        # Params
        mu0 <- 0
        power <- -1
        N <- -1
        crit_output <- ''
        two_sided <- as.numeric(input$two_sided)
        user_effect <- abs(input$muA)/input$sd
        # Calc power or sample size based on input
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == "2",
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
            N <- ifelse(input$sd_known == "2",
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
        if(input$sd_known == "2")
        {
            crit_val <- qt(1 - input$alpha / two_sided, df = N-1)
            crit_output <- sprintf("T Critival Value: %s %s", ifelse(two_sided == 2, "-/+", ''), round(crit_val, 3))
        }else{
            if(two_sided == 2){
                # Upper and lower
                crit_val_lwr <- qnorm(input$alpha/two_sided, mean = mu0, sd = input$sd/sqrt(N))
                crit_val_upr <- qnorm(1 - input$alpha/two_sided, mean = mu0, sd = input$sd/sqrt(N))
                crit_output <- sprintf("Critical Values: %s, %s", round(crit_val_lwr, 3), round(crit_val_upr, 3))
            }
            else{
                # lower or upper
                if(mu0 < input$muA)
                {
                    crit_val <- qnorm(1 - input$alpha, mean = mu0, sd = input$sd/sqrt(N))
                    crit_output <- sprintf("Critical Value: %s", round(crit_val, 3))
                }
                else
                {
                   crit_val <- qnorm(input$alpha, mean = mu0, sd = input$sd/sqrt(N))
                   crit_output <- sprintf("Critical Value: %s", round(crit_val, 3))
                }
            }
        }
        
        # return vars
        distribution <- paste('Distribution:', ifelse(input$sd_known == "2", 'Student\'s t', 'Normal'))
        effect_size <- paste("Effect Size:", round(user_effect, 3))
        power_output <- paste("Power:", round(power, 3))
        N_output <- paste("Sample Size:", N)
        
        # Add DF if T distr
        if(input$sd_known == "2")
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
        mu0 <- 0
        power <- -1
        N <- -1
        effect_size <- abs(input$muA)/input$sd
        two_sided <- as.numeric(input$two_sided)
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == "2",
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
            N <- ifelse(input$sd_known == "2",
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
        if(input$sd_known == "2")
        {
            # T distribution
            df <- N - 1
            test_stat <- qt(1 - input$alpha / two_sided, df = df)
            ncp <- abs(input$muA) * sqrt(N)/input$sd
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
            test_stat <- qnorm(1 - input$alpha/two_sided, mean = mu0, sd = distr_sd)
            # neg_test_stat <- ifelse(mu0 != 0, qnorm(input$alpha/two_sided, mean = input$mu0, sd = distr_sd), -test_stat)
            neg_test_stat <- -test_stat

            # mu0 <= muA
            if(mu0 <= input$muA){
                upr_h0 <- mu0 + 3.5*distr_sd
                lwr_bound <- mu0 - 3.5*distr_sd
                upr_bound <- input$muA + 3.5*distr_sd
                seq_by <- ifelse(upr_h0 - lwr_bound < 2, 0.001, 0.01)
                data <- tibble(x_val = seq(from = lwr_bound,
                                           to = upr_bound,
                                           by = seq_by),
                               pdf_h0 = dnorm(x_val, mean = mu0, sd = distr_sd),
                               pdf_hA = dnorm(x_val, mean = input$muA, sd = distr_sd))
                if(two_sided == 2){
                    two_sided_z_h0_leq(data, mu0, input$muA, test_stat, neg_test_stat, color)
                } else{
                    one_sided_z_h0_leq(data, mu0, input$muA, test_stat, color)
                }
            } else {
                # mu0 > muA
                upr_hA <- input$muA + 3.5*distr_sd
                lwr_bound <- input$muA - 3.5*distr_sd
                upr_bound <- mu0 + 3.5*distr_sd
                seq_by <- ifelse(upr_hA - lwr_bound < 2, 0.001, 0.01)
                data <- tibble(x_val = seq(from = lwr_bound,
                                           to = upr_bound,
                                           by = seq_by),
                               pdf_h0 = dnorm(x_val, mean = mu0, sd = distr_sd),
                               pdf_hA = dnorm(x_val, mean = input$muA, sd = distr_sd))
                if(two_sided == 2){
                    two_sided_z_h0_greater(data, mu0, input$muA, test_stat, neg_test_stat, color)
                } else{
                    one_sided_z_h0_greater(data, mu0, input$muA, neg_test_stat, color)
                }
            }
        }
    }, height = 555)

    #Power curve
    output$pow_curve <- renderPlot({
        power <- -1
        N <- -1
        mu0 <- 0
        effect_size <- round(abs(input$muA)/input$sd, 3)
        two_sided <- as.numeric(input$two_sided)
        if(as.numeric(input$which_calc) == 1){
            power <- ifelse(input$sd_known == "2",
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
            N <- ifelse(input$sd_known == "2",
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
            if(input$sd_known == "2"){
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
            if(input$sd_known == "2"){
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
