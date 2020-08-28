library(shiny)
library(dplyr)
library(Matrix)
library(mosaic)
library(ggplot2)
library(DT)


# Helper Functions -------------------------------------------------------------
#-------------------------------------------------------------------------------
#  Title: Confidence Intervals for Binomial Probability
#  Author: Frank E. Harrell, Jr.
#  Date: 2020
#  Code version: 4.4-0
#  Availability: https://CRAN.R-project.org/package=Hmisc
#  We needed to bypass the loading of the foreign package for R 3.6.3, thus
#  we are using the definition of the binconf which is all we needed from Hmisc.
#-------------------------------------------------------------------------------
binconf <- function (x, n, alpha = 0.05,
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



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "UPRes")
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
        cover = if_else(input$base == "True Proportion", ((lowerbound < input$prop) &
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
  ############################################################
  ############################################################
  
  
  
})
