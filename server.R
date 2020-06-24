library(shiny)
library(dplyr)
library(Matrix)
library(mosaic)
library(ggplot2)
library(DT)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "UPRes")
  })
  #Null hypothesis
  output$nullhypo = renderUI({
    h3("Ho: p = 0.583")
  })
  
  output$design = renderUI({
    if (input$designcheckbox)
    {
      h4(
        "A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study. This app shows  how confidence intervals of that type would come out when there is no bias."
      )
    }
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
        title = paste0("Study result is ", input$prop),
        x = "Whether PA Resident",
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
    
    # validate(
    #   need(input$nsamp >=30,
    #        message = "Please input samle size larger than 30")
    # )
    
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
        x = "Whether PA Resident",
        y = "Proportion Enrollment by Residency"
      )
    #barplot(my_vector,col=rgb(0.2,0.4,0.6,0.6),ylim=c(0,1), ylab="precentage")
    
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
  
  # text messages
  output$CoverageRate <- renderText({
    validate(need(is.numeric(input$nsamp),
                  message = "Please input sample size"))
    
    paste(
      sum(Intervals()$cover),
      "of these",
      nrow(Intervals()),
      "intervals cover the parameter value. And coverage rate is ",
      round(100 *  sum(Intervals()$cover) / nrow(Intervals()), 2),
      "% (",
      rate$total,
      " samples)"
    )
  })
  ############################################################
  ############################################################
  
  
  
})
