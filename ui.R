library(shiny)
library(shinydashboard)
library(shinyBS)


dashboardPage(
  skin = "purple",
  
  #Title
  dashboardHeader(
    title = "Non-response Bias",
    titleWidth = 250,
    
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/',
                   icon("home")))
  ),
  
  #Sidebar
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                     menuItem(
                       "UP Residency Percentage",
                       tabName = "UPRes",
                       icon = icon("wpexplorer")
                     ),
                     menuItem("References",tabName = "Ref",icon = icon("leanpub"))),
                     #PSU logo
                     tags$div(
                       class = "sidebar-logo",
                       boastUtils::psu_eberly_logo("reversed")
                     )
                     
                   ),
  
  #Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
                href="https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    
    
    tabItems(
      tabItem(
        tabName = "over",
        
        
        h1("Non-Response Bias"),
        p("This app visualizes how the confidence level and sample size affect the outcome confidence interval for a single proportion. The app also explores  non-response bias when respondents are chosen from a population with a different true proportion causing non-response bias."),
        br(),
        h2("Instructions"),
       
        tags$ol(
          tags$li(
            "Move the sample size and level sliders to see how they affect confidence intervals for the proportion of Penn State students who are residents of Pennsylvania."
          ),
          tags$li(
            "Move the true proportion slider to see the effects of non-response bias."
          )
        ),
        
        div(
          style = "text-align: center",
          bsButton(
            "explore",
            "Explore",
            icon("bolt"),
            size = "large",
            class = "circle grow",
            style = "default"
          )
        ),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and programmed by Zhuolin Luo extending earlier work by  Yingjie (Chelsea) Wang."),
        
        br(),
        p("We would like to extend a special thanks to the Shiny Program Students.",
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 06/22/2020 by ZL.")
        )
      ),
      
      
      tabItem(tabName = "UPRes",
              fluidPage(
                titlePanel(
                  "Confidence Intervals for Enrollment by Residency in 2019 (p = 58.3%)"
                ),
                box(
                  title = strong(tags$em("Design:")),
                  # This is the header of the box. Consider using "Story Context"
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = '100%',
                  "A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the population of students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the null hypothesis proportion of 0.583 for the population of all Penn State University Park students. This app shows how confidence intervals of that type would come out when there is bias so the proportion for the population of volunteer participants is really different."
                )
                ,
                sidebarLayout(
                  sidebarPanel(
                    # h3(strong("Confidence Intervals for a population mean (n > 30): ")),
                    # h4("For large random samples a confidence interval for a population mean is given by"),
                    # h3("sample mean ± z* s / sqrt(n)"),
                    # h4("where z* is a multiplier number that comes form the normal curve and determines the level of confidence."),
                    
                    #h4("A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the mean of 59.5% for the population of all Penn State University Park students. This app shows  how confidence intervals of that type would come out when there is no bias."),
                    #h4("TIP: Click on an interval to show a histogram for the underlying sample."),
                    h3(strong("Hypothesis: ")),
                    uiOutput("nullhypo"),
                    
                    sliderInput(
                      "prop",
                      "True Proportion for Pennsylvania Students in UP",
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
                    ),
                    br(),
                    br()
                    
                    
                    
                  ),
                  
                  mainPanel(
                    fluidRow(
                      splitLayout(
                        cellWidths = c("50%", "50%"),
                        plotOutput("popMean", height = "350px"),
                        plotOutput("sampProp", height = "350px")
                      ),
                      wellPanel(
                        plotOutput("CIplot", height = "600px", click = "plot_click"),
                        textOutput("CoverageRate")
                      ),
                      br(),
                      bsPopover(
                        "sampProp",
                        "Sample Histogram",
                        "This is the histogram plot of the sample you selected on Confidence Interval Plot. The orange line is the true proportion that you input.",
                        trigger = "hover",
                        placement = "top"
                      ),
                      br(),
                      
                      bsPopover(
                        "popMean",
                        "Population Bar Graph",
                        "This is the bar plot based on proportion you input. The green line is the null hypothesis perventage and also the true proportion in 2019.",
                        trigger = "hover",
                        placement = "bottom"
                      ),
                      br(),
                      
                      bsPopover(
                        "CIplot",
                        "Confidence Interval Plot",
                        "The blue lines indicate a confidence interval covers the proportion selected and the red lines indicate that the selected proportion is outside of the confidence interval. Click on an interval to show a histogram for the underlying sample.",
                        trigger = "hover",
                        placement = "top"
                      ),
                      br(),
                      
                      
                      tags$head(
                        tags$style(
                          "#CoverageRate{color: green;
                                 font-size: 18px;
                                                     font-style: italic;
                                                     }"
                        )
                      )
                    )
                  )
                )
              )),
      
      tabItem(
        tabName = "Ref",
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

