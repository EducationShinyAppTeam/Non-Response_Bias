# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(dplyr)
library(Matrix)
library(mosaic)
library(ggplot2)
library(DT)

# Define Constants and Functions ----
#-------------------------------------------------------------------------------
#  Title: Confidence Intervals for Binomial Probability
#  Author: Frank E. Harrell, Jr.
#  Date: 2020
#  Code version: 4.4-0
#  Availability: https://CRAN.R-project.org/package=Hmisc
#  We needed to bypass the loading of the foreign package for R 3.6.3, thus
#  we are using the definition of the binconf which is all we needed from Hmisc.
#-------------------------------------------------------------------------------
binconf <- function(x, n, alpha = 0.05,
                     method = c("wilson", "exact", "asymptotic", "all"),
                     include.x = FALSE, include.n = FALSE, return.df = FALSE)
{
  method <- match.arg(method)
  bc <- function(x, n, alpha, method) {
    nu1 <- 2 * (n - x + 1)
    nu2 <- 2 * x
    ll <- if (x > 0)
      x/(x + qf(1 - alpha/2, nu1, nu2) * (n - x + 1))
    else 0
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (x < n)
      qf(1 - alpha/2, nu1p, nu2p)
    else 1
    ul <- ((x + 1) * pp)/(n - x + (x + 1) * pp)
    zcrit <- -qnorm(alpha/2)
    z2 <- zcrit * zcrit
    p <- x/n
    cl <- (p + z2/2/n + c(-1, 1) * zcrit *
             sqrt((p * (1 - p) + z2/4/n)/n))/(1 + z2/n)
    if (x == 1)
      cl[1] <- -log(1 - alpha)/n
    if (x == (n - 1))
      cl[2] <- 1 + log(1 - alpha)/n
    asymp.lcl <- x/n - qnorm(1 - alpha/2) * sqrt(((x/n) *
                                                    (1 - x/n))/n)
    asymp.ucl <- x/n + qnorm(1 - alpha/2) * sqrt(((x/n) *
                                                    (1 - x/n))/n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(x/n, 3), res)
    switch(method, wilson = res[2, ], exact = res[1, ], asymptotic = res[3,
    ], all = res, res)
  }
  if ((length(x) != length(n)) & length(x) == 1)
    x <- rep(x, length(n))
  if ((length(x) != length(n)) & length(n) == 1)
    n <- rep(n, length(x))
  if ((length(x) > 1 | length(n) > 1) & method == "all") {
    method <- "wilson"
    warning("method=all will not work with vectors...setting method to wilson")
  }
  if (method == "all" & length(x) == 1 & length(n) == 1) {
    mat <- bc(x, n, alpha, method)
    dimnames(mat) <- list(c("Exact", "Wilson", "Asymptotic"),
                          c("PointEst", "Lower", "Upper"))
    if (include.n)
      mat <- cbind(N = n, mat)
    if (include.x)
      mat <- cbind(X = x, mat)
    if (return.df)
      mat <- as.data.frame(mat)
    return(mat)
  }
  mat <- matrix(ncol = 3, nrow = length(x))
  for (i in 1:length(x)) mat[i, ] <- bc(x[i], n[i], alpha = alpha,
                                        method = method)
  dimnames(mat) <- list(rep("", dim(mat)[1]), c("PointEst",
                                                "Lower", "Upper"))
  if (include.n)
    mat <- cbind(N = n, mat)
  if (include.x)
    mat <- cbind(X = x, mat)
  if (return.df)
    mat <- as.data.frame(mat, row.names = NULL)
  mat
}
#End of Harrell's code----------------------------------------------------------

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Non-Response Bias",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Non-Response_Bias")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = 'https://shinyapps.science.psu.edu/',
          icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("UP Residency Percentage", tabName = "UPRes", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references",icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          h1("Non-Response Bias"),
          p("This app visualizes how the confidence level and sample size affect
            the  outcome confidence interval for a single proportion. The app
            also explores non-response bias when respondents are chosen from a
            population with a different true proportion causing non-response
            bias."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li(
              "Move the sample size and level sliders to see how they affect
              confidence intervals for the proportion of Penn State students
              who are residents of Pennsylvania."
            ),
            tags$li(
              "Move the true proportion slider to see the effects of
              non-response bias."
            )
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "explore",
              label = "Explore",
              icon = icon("bolt"),
              size = "large"
            )
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and programmed by Zhuolin Luo extending
            earlier work by  Yingjie (Chelsea) Wang.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 05/17/2021 by NJH.")
          )
        ),
        ## UP Residency Exploration Page ----
        tabItem(
          tabName = "UPRes",
          h2("Confidence Intervals for Enrollment by Residency in 2019 (p = 58.3%)"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "A researcher plans to take a random sample of size n students to
          do a survey about their experiences in studying at the University Park
          campus of Penn State University. However, she worries that sample results
          could be biased because the population of students who agree to participate
          might be different from those who don't (this would be an example of non-response bias).
          The researcher makes a confidence interval for the percentage of Penn State Students
          who are Pennsylvania residents based on her study and compares it to
          the null hypothesis proportion of 0.583 for the population of all Penn State
          University Park students. This app shows how confidence intervals of that
          type would come out when there is bias so the proportion for the population
          of volunteer participants is really different."
          ),
          fluidRow(
            column(4,
              offest = 0,
              h3("Hypothesis"),
              uiOutput("nullhypo"),
              sliderInput(
                "prop",
                "True Proportion for volunteer population",
                min = 0.01,
                max = 0.99,
                value = 0.583,
                step = 0.01
              ),
              sliderInput(
                "level",
                "Confidence Level",
                min = .50,
                max = 0.99,
                value = 0.90,
                step = 0.01
              ),
              sliderInput(
                "nsamp",
                "Sample Size (n > 30)",
                min = 30,
                max = 500,
                value = 30,
                step = 5
              ),
              selectInput(
                "base",
                "Select Basis of CI coverage color code",
                selected = "True Proportion",
                choices = c("Null Hypothesis Proportion", "True Proportion")
              ),
              actionButton("new", "Generate 50 New Samples", icon("retweet")),
              bsPopover(
                "new",
                "Note",
                "Click to generate 50 new samples, each with the sample size you have input.",
                trigger = "hover",
                placement = "right"
              )
            ),
            column(4,
                   plotOutput("popMean", height = "450px"),
                   tags$script(HTML(
                     "$(document).ready(function() {
                  document.getElementById('popMean').setAttribute('aria-label',
                  `Population proportion`)
                  })"
                   )),
                  bsPopover(
                    "popMean",
                    "Population Bar Graph",
                    "This is the bar plot based on proportion you input.",
                    trigger = "hover",
                    placement = "bottom"
                  )
            ),
            column(4,
                   plotOutput("sampProp", height = "450px"),
                   tags$script(HTML(
                     "$(document).ready(function() {
                  document.getElementById('sampProp').setAttribute('aria-label',
                  `Sample proportion`)
                  })"
                   )),
                  bsPopover(
                    "sampProp",
                    "Sample Bar Graph",
                    "This is the bar graph of the sample you selected on Confidence Interval Plot.",
                    trigger = "hover",
                    placement = "top"
                  )
            )
          ),
          br(),
          fluidRow(
            textOutput("orangeline"),
            textOutput("greenline")
          ),

          br(),
          fluidRow(
            plotOutput("CIplot", height = "750px", click = "plot_click"),
            tags$script(HTML(
              "$(document).ready(function() {
           document.getElementById('CIplot').setAttribute('aria-label',
           `Confidence interval plot`)
           })"
            )),
           textOutput("CoverageRate"),
           bsPopover(
             "CIplot",
             "Confidence Interval Plot",
             "The blue lines indicate a confidence interval covers the proportion selected
           and the red lines indicate that the selected proportion is outside of the confidence interval.
           Click on an interval to show a bar graph for the underlying sample.",
           trigger = "hover",
           placement = "top"
           ),
           bsPopover(
             "CIplot",
             "Confidence Interval Plot",
             "The blue lines indicate a confidence interval covers the proportion
           selected and the red lines indicate that the selected proportion is
           outside of the confidence interval. Click on an interval to show a
           histogram for the underlying sample.",
           trigger = "hover",
           placement = "top"
           ),
           tags$head(
             tags$style(
               "#CoverageRate{color: green;
                                 font-size: 18px;
                                                     font-style: italic;
                                                     }"
             )
           )
          )
        ),
        ## References ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package. Available from https://CRAN.R-project.org/package=shinyBS"),
          p(class = "hangingindent",
            "Bates D. and Maechler M. (2019), Matrix: Sparse and Dense Matrix Classes and Maethods, R package. Available from https://cran.r-project.org/web/packages/Matrix/index.html"),
          p(class = "hangingindent",
            "Budget Office in PSU. (2019), Enrollment by Residency Fall 2019. Available at https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N"),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create dashboards with 'Shiny', R Package. Available from https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019), shiny: Web application framework for R, R Package. Available from https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "Harrell, F.E. (2020), Confidence Intervals for Binomial Probability. We needed to bypass the loading of the foreign package for R 3.6.3, thus we are using the definition of the binconf which is all we needed from Hmisc. Available from https://CRAN.R-project.org/package=Hmisc"),
          p(class = "hangingindent",
            "Pruim, R., Kaplan, D.T., and Horton, N.J. (2020), mosaic: Project MOSAIC statistics and mathematics teaching utilities, R Package. Avaliable from https://CRAN.R-project.org/package=mosaic"),
          p(class = "hangingindent",
            "Wickham H., Francois R., Henry L., and Muller K. (2020), dplyr: A Grammar of Data Manipulation, R Package. Available from https://cran.r-project.org/web/packages/dplyr/index.html"),
          p(class = "hangingindent",
            "Wickham H. (2016), ggplot2: Elegant graphics for data analysis, R Package, New York: Springer-Verlag. Available from https://ggplot2.tidyverse.org"),
          p(class = "hangingindent",
            "Wickham, H., Seidel, D., and R Studio. (2020), scales: Scale function for visualization, R Package. Availabel from https://CRAN.R-project.org/package=scales"),
          p(class = "hangingindent",
            "Xie, Y. (2020), DT: A wrapper of the JavaScript library 'DataTables', R Package. Available from https://CRAN.R-project.org/package=DT")
        )
      )#end of tabItem
    )#end of dashboardBody
  )
)

# Define server logic
server <- function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "UPRes")
  })

  #population plot with true prop
  output$popMean  = renderPlot({
    my_vector = c(0.583, 1 - 0.583)
    names(my_vector) = c("PA Students", "Non-PA Students")
    ggplot() + geom_bar(
      aes(x = names(my_vector), y = my_vector),
      stat = 'identity',
      width = 0.3,
      fill = "steelblue"
    ) +
      lims(y = c(0, 1)) +
      geom_hline(yintercept = 0.583,
                 color = "forestgreen",
                 size = 1.2) +
      geom_hline(yintercept = input$prop,
                 size = 1.2,
                 colour = "orange") +
      labs(
        title = paste0("True proportion for volunteer population is ", input$prop),
        x = "Pennsylvania residency status",
        y = "Proportion Enrollment by Residency"
      )
    #barplot(my_vector,col=rgb(0.2,0.4,0.6,0.6),ylim=c(0,1), ylab="precentage")

  })



  #Calculating alpha by the confidence level input
  alpha <- reactive({
    (1 - input$level)
  })

  #Updating Sample Size
  N <- reactive({
    as.integer(input$nsamp)
  })


  #generate 50 new sample
  Data <- reactive({
    input$new
    data.frame(x =
                 do.call(paste0("rbinom"),
                         c(
                           list(n = as.integer(input$nsamp) * 50), list(1, input$prop)
                         ))) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })

  #calculate the interval

  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        Count = sum(x),
        sampleProp = binconf(Count, N(), alpha = alpha())[1],
        lowerbound = binconf(Count, N(), alpha = alpha())[2],
        upperbound = binconf(Count, N(), alpha = alpha())[3],
        cover
        = if_else(input$base == "True Proportion", ((lowerbound < input$prop) &
                                                      (input$prop < upperbound)
        ), ((lowerbound < 0.583) & (0.583 < upperbound)
        ))
      ) %>%
      ungroup()
  })


  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (!is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1)
        selected_sample <<- 1
      if (selected_sample > 50)
        selected_sample <<- 50
    }
    selected_sample
  })

  OneSample <- reactive({
    Data() %>%
      filter(idx == selectedSample())
  })

  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <-
      (Intervals() %>% filter(idx == selectedSample()))$cover
    colors[as.character(covers)]
  })

  #print the CIplot
  output$CIplot <- renderPlot({
    validate(need(is.numeric(input$nsamp),
                  message = "Please input sample size"))
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(
          x = idx,
          ymin = lowerbound,
          ymax = upperbound,
          y = sampleProp,
          colour = cover,
          alpha = idx == selectedSample(),
          size = idx == selectedSample()
        )
      ) +
      geom_hline(
        yintercept = 0.583,
        size = 2,
        colour = "forestgreen",
        alpha = 0.5
      ) +
      geom_hline(
        yintercept = input$prop,
        size = 2,
        colour = "orange",
        alpha = 0.5
      ) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7),
                        guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"),
                         guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5),
                         guide = FALSE) +
      lims(y = c(-0.01, 1.01)) +
      labs(
        title = paste0(100 * input$level, "% Confidence Intervals for the proportion"),
        x = "50 samples are generated every time",
        y = "green vertical line shows null proportion, orange vertical line shows true proportion"
      ) +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })

  output$sampProp  = renderPlot({
    validate(need(is.numeric(input$nsamp),
                  message = "Please input sample size"))
    my_vector = c(round(mean(OneSample()$x), 2), 1 - round(mean(OneSample()$x), 2))
    names(my_vector) = c("PA Students", "Non-PA Students")
    ggplot() + geom_bar(
      aes(x = names(my_vector), y = my_vector),
      width = 0.3,
      stat = 'identity',
      fill = OneSampleColor()
    ) +
      lims(y = c(0, 1)) +
      geom_hline(yintercept = 0.583,
                 color = "forestgreen",
                 size = 1.2) +
      geom_hline(yintercept = input$prop,
                 color = "orange",
                 size = 1.2) +
      labs(
        title = paste0("Sample proportion for residency in UP = ",
                       round(mean(OneSample(
                       )$x), 2)),
        x = "Pennsylvania residency status",
        y = "Proportion Enrollment by Residency"
      )


  })

  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })


  observeEvent(c(input$A, input$B, input$n, input$level),
               {
                 rate$cover <-
                   sum(Intervals()$cover)
                 rate$total <- nrow(Intervals())
               })

  # information about the lines
  output$orangeline <- renderText({
    paste0("The orange line is the true proportion that you input.")
  })

  output$greenline <- renderText({
    paste0("The green line is the null hypothesis percentage and also the true proportion in 2019.")
  })

  output$CoverageRate <- renderText({
    validate(need(is.numeric(input$nsamp),
                  message = "Please input sample size"))

    paste(
      sum(Intervals()$cover),
      "of these",
      nrow(Intervals()),
      "intervals cover the null hypothesis parameter value. And coverage rate is",
      round(100 * sum(Intervals()$cover) / nrow(Intervals()), 2),
      "% (50 samples). "
    )
  })
}

# Boast App Call ----
boastApp(ui = ui, server = server)