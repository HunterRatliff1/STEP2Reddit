
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# ---- Packages
require(shiny)
suppressPackageStartupMessages(require(tidyverse))
require(ggthemes)
require(lubridate)
require(stringr)
require(ggrepel)
require(readr)


# ---- Load data
reddit <- readr::read_csv("Step2CKSurvey.csv", na = c("", "NA", "#N/A")) 

reddit <- reddit %>%
  mutate(
    Date_StudyStart = mdy(Date_StudyStart), # Sys.setenv(TZ="America/Chicago")   
    STEP2.date      = mdy(STEP2.date),
    UW_FirstPass    = UW_FirstPass/100
  ) %>% 
  rename(UW_FP=UW_FirstPass)



shinybootstrap2::withBootstrap2(shinyUI(fluidPage(

  # Application title
  titlePanel("STEP 2 CK Score Linear Model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        shiny::sliderInput("ci", "Confidence interval", 50, 100, 95, 1),
        h3("Your scores"),
        numericInput("UW_1", label="UWSA 1", value = 251),
        numericInput("UW_2", label="UWSA 2", value = NA),
        numericInput("UW_FP", label="UW First pass (%)", value = NA, min=1, max=100),
        numericInput("Goal", label="Goal/target score", value = NA),
        numericInput("NBME_6", label="NBME 6", value = NA),
        numericInput("NBME_7", label="NBME 7", value = NA),
        numericInput("NBME_8", label="NBME 8", value = NA)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(6, 
          # checkboxGroupInput("modelVars", h3("Score(s) to include in model"), 
          #                    inline=F,  selected = c("UWSA 1"="UW_1"),
          #                    choices = c("UWSA 1"="UW_1", "UWSA 2"="UW_2", 
          #                                "UW First pass"="UW_FP", "Goal score"="Goal",
          #                                "NBME #6"="NBME_6", "NBME #7"="NBME_7", 
          #                                "NBME #8"="NBME_8"))
          uiOutput("dynamicCheckbox"),
          hr(),
          plotOutput("graphDrop", height = "75px"),
          p("Based on", strong(textOutput("numDrop", inline=T)), " total survey responses")
               ),
        column(6, 
               p("Below are the calculated scores based on the equations from Reddit:"),
               htmlOutput("redditModel"),
               
               modalDialog(title="How this works", size="m", easyClose=T,
                 p("First, input your scores from all of the practice tests you've taken",
                   "in their respective boxes on the left of the page."), 
                 p("You can then generate a linear model that predicts your score by checking the",
                   "checkboxes at the top of the page for whichever practice tests you want to include.",
                   strong("Entering your scores in the sidebar does", tags$u("not"),
                   "include them in the model;"),"you must", strong("use the checkboxes."),
                   "The confidence interval can be adjusted using the slider in the left-upper corner"),
                 hr(),
                 p("Keep in mind that there is a tradeoff between a more complex model and",
                   "the sample size. More complex models (e.g. including all five practice tests)", 
                   "have fewer responses included, because they only include survey responses from",
                   "people who took all five practice tests (in this case, only 15% of the dataset).",
                   "So it's best to stick to one or two variables (e.g.", code("UW_1"),"+", code("UW_2"), 
                   ", which includeds 80% of the dataset)."
                   )
                   
                 )
                           
        )
      ),
      hr(),
      
      p("Using the model formula", strong("STEP2 ="), code(textOutput("modFormula", inline=T)), " as input:"),
      h4("Estimated score: ", strong(style="color:red", textOutput("estScore", inline=T)),
         textOutput("estCI", inline=T)),
      
      
      plotOutput("graph"),
      p("Your projected score is in red. The grey points/bars represent the data from the reddit
        survey for comparison"),
      hr(),
      h3("Summary of linear model"),
      verbatimTextOutput("modSummary")
    )
  )
)))
