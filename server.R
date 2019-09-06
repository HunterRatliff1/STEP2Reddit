
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny)
suppressPackageStartupMessages(require(tidyverse))
require(ggthemes)
require(lubridate)
require(stringr)
require(ggrepel)
require(scales)

# ---- Load data
reddit <- readr::read_csv("Step2CKSurvey.csv", na = c("", "NA", "#N/A")) 

reddit <- reddit %>%
  mutate(
    Date_StudyStart = mdy(Date_StudyStart), # Sys.setenv(TZ="America/Chicago")   
    STEP2.date      = mdy(STEP2.date),
    UW_FirstPass    = UW_FirstPass/100
  ) %>% 
  rename(UW_FP=UW_FirstPass)

shinyServer(function(input, output) {
  
  
  
  # Dynamic UI checkboxes
  output$dynamicCheckbox <- renderUI({
    options <- c("UWSA 1"="UW_1", "UWSA 2"="UW_2",
                 "UW First pass"="UW_FP", "Goal score"="Goal",
                 "NBME #6"="NBME_6", "NBME #7"="NBME_7",
                 "NBME #8"="NBME_8")
    toRemove <- c()
    
    if(!is.numeric(input$UW_1)) {toRemove <- c(toRemove, "UWSA 1"="UW_1")}
    if(!is.numeric(input$UW_2)) {toRemove <- c(toRemove, "UW_2")}
    if(!is.numeric(input$UW_FP)) {toRemove <- c(toRemove, "UW_FP")}
    if(!is.numeric(input$Goal)) {toRemove <- c(toRemove, "Goal")}
    if(!is.numeric(input$NBME_6)) {toRemove <- c(toRemove, "NBME_6")}
    if(!is.numeric(input$NBME_7)) {toRemove <- c(toRemove, "NBME_7")}
    if(!is.numeric(input$NBME_8)) {toRemove <- c(toRemove, "NBME_8")}
    
    options <- options[ - which(options %in% toRemove)]
    
    # Render UI
    checkboxGroupInput("modelVars", h3("Score(s) to include in model"),
                       inline=F,  selected = options,
                       choices = options)
  })
  
  
  
  # ----- REACTIVES
  modelObj <- reactive({  # Expression that generates the model
    lm(as.formula(paste0(
      "STEP2 ~ ", paste(input$modelVars, collapse="*"),"- 1")), data = reddit)
  })
  
  inScores <- reactive({  # Inputs from user of scores
    in.UW_1   <- ifelse(is.integer(input$UW_1), input$UW_1, NA)
    in.UW_2   <- ifelse(is.integer(input$UW_2), input$UW_2, NA)
    in.UW_FP  <- ifelse(is.integer(input$UW_FP), input$UW_FP/100, NA)
    in.NBME_6 <- ifelse(is.integer(input$NBME_6), input$NBME_6, NA)
    in.NBME_7 <- ifelse(is.integer(input$NBME_7), input$NBME_7, NA)
    in.NBME_8 <- ifelse(is.integer(input$NBME_8), input$NBME_8, NA)
    in.Goal   <- ifelse(is.integer(input$Goal), input$Goal, NA)
    
    inputScores <- data_frame(x = c("Projected"))
    if("UW_1" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, UW_1 = c(in.UW_1))
    } 
    if("UW_2" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, UW_2 = c(in.UW_2))
    }
    if("UW_FP" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, UW_FP = c(in.UW_FP))
    }
    if("NBME_6" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, NBME_6 = c(in.NBME_6))
    }
    if("NBME_7" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, NBME_7 = c(in.NBME_7))
    }
    if("NBME_8" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, NBME_8 = c(in.NBME_8))
    }
    if("Goal" %in% input$modelVars) {
      inputScores <- bind_cols(inputScores, Goal = c(in.Goal))
    }
    
    # Predicton (https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals)
    inputScores %>%
      bind_cols() %>%
      data.frame(
        predict(modelObj(), inputScores, interval = "prediction", level=input$ci/100)
      ) %>% 
      mutate(
        fit = round(fit),
        lwr = round(lwr),
        upr = round(upr)
      ) %>%
      rename(`EstScore`=fit, `LowerCI`=lwr, `UpperCI`=upr) # return df
    
    
  })
  
  # ---- Help modal
  observeEvent(input$help, {
    showModal(modalDialog(title="How this works", size="m", easyClose=T,
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
                
    ))
  })
  
  # ---- Graph
  output$graph <- renderPlot({  # Prints test stuff
    df<-inScores()
    estScore <- df["EstScore"][[1]]
    lci <- df["LowerCI"][[1]]
    uci <- df["UpperCI"][[1]]
    
    if(length(input$modelVars)>1) {
      reddit %>%
        filter(STEP2>200) %>%
        ggplot() + 
        geom_histogram(aes(x=STEP2), alpha=0.5, binwidth=1) +
        geom_point(aes(x=EstScore, y=10), data=df, color="red") +
        geom_label_repel(aes(x=EstScore, y=10, label="Your projected score"), 
                         box.padding = unit(0.5, "lines"), data=df) + 
        geom_errorbarh(aes(y=10, xmin=LowerCI, xmax=UpperCI), data=df, color="red") +
        theme_fivethirtyeight() + 
        theme(axis.title=element_text())+
        labs(x="STEP 2 CK score")
    } else {
      df %>%
        ggplot(aes(x=!!sym(input$modelVars), y=EstScore)) +
        geom_jitter(aes(x=!!sym(input$modelVars), y=STEP2), alpha=0.5,
                   data=filter(reddit, STEP2>200)) +
        geom_smooth(aes(x=!!sym(input$modelVars), y=STEP2), alpha=0.5,
                    data=filter(reddit, STEP2>200), color="black") +
        geom_point(color="red") +
        geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), color="red") +
        geom_label_repel(aes(label="Your projected score"), 
                         box.padding = unit(0.5, "lines"), data=df) + 
        theme_fivethirtyeight() +
        theme(axis.title=element_text())+
        labs(y="STEP 2 CK score")
    }
  })
  
  # ---- Dropped bar
  output$graphDrop <- renderPlot({
    total <- nrow(filter(reddit, STEP2>200))
    missing <- total - nobs(modelObj())
    
    data_frame(
      Category=c("included", "excluded"), 
      n=c(total-missing,missing)) %>% 
      
      mutate(n = n/sum(n)) %>%
      ggplot(aes(x="", y=n, fill=Category)) + 
      geom_col(width=1) + 
      coord_flip() + theme_fivethirtyeight() +  
      scale_fill_manual(values = c("red", "black")) + 
      scale_y_continuous(labels=percent) +
      guides(fill="none") +
      theme(plot.background=element_rect(fill="white"), panel.background=element_rect(fill="white")) +
      labs(title=paste0(missing, " (",scales::percent(missing/total),") omitted"))
  })
  
  
  
  
  
  # ---- Text reactives
  output$complexWarning <- renderUI({
    if(length(input$modelVars) > 3){
     div(class="alert alert-danger", "Warning: Including too many variables in the model makes it act funny,
      and likely decreases it's accuracy")
    }
  })
  
  output$estScore <- renderText({  # Estimated score
    df<-inScores()
    df["EstScore"][[1]]
  })
  output$estCI <- renderText({     # Lower & upper CI
    df<-inScores()
    lci <- df["LowerCI"][[1]]
    uci <- df["UpperCI"][[1]]
    paste0("(",lci," - ",uci,")")
  })
  
  output$numDrop <- renderText({  # Outputs number of observations dropped
    nobs <- nobs(modelObj())
    total <- nrow(filter(reddit, STEP2>200))
    paste0(nobs, " of ", total)
  })
  
  output$redditModel <- renderUI({  # output scores from reddit's equations
    
    df <- data_frame(
      NameOfTest = c("UWSA 1", "UWSA 2", "UW first pass", 
                     "NBME 6", "NBME 7", "NBME 8"),
      Test = c("UW_1","UW_2","UW_FP","NBME_6","NBME_7","NBME_8"),
      m    = c(0.5942,0.7861,1.1213,0.3944,0.5959,0.3564),
      b    = c(105.08,56.463,172.48,160.77,114.48,168.68),
      x    = c(input$UW_1, input$UW_2, input$UW_FP,
               input$NBME_6, input$NBME_7, input$NBME_8)
    ) %>%
      filter(x>0) %>%
      # filter(Test %in% input$modelVars) %>%
      mutate(ProjScore = round(m*x + b)) %>%
      select(NameOfTest, ProjScore) %>%
      mutate(TextOut = paste0("<u>",NameOfTest,"</u>: ",ProjScore,"<br>"))
    
    textOut <- str_c(df$TextOut)
    HTML(textOut)
    
  })
  
  output$modFormula <- renderText({  # Outputs the selected formula as text
    paste(input$modelVars, collapse=" + ")
  })
  
  output$coeffDf <- renderTable(digits=5, {  # Outputs the coefficients of model as table
    mod <- modelObj() 
    x <- summary(mod)
    as.data.frame(x$coefficients) %>% 
      rownames_to_column("Variable") %>% 
      rename(`p value`=`Pr(>|t|)`)
  })
  
  output$modSummary <- renderPrint({  # Prints raw summary of model
    mod <- modelObj() 
    summary(mod)  # pander::pander()
  })

})
