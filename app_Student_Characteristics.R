
library(shiny)
library(DT)
library(scales)
library(tidyverse)
library(tidyr)
library(janitor)
library(readr)
library(arules)
library(arulesViz)
library(lubridate)
library(plotly)
library(gt)
library(gtsummary)
library(RColorBrewer)

 
student_character_1stFall_df <- readRDS("student_character_1stFall_df.rds") # by term
 

ui <- fluidPage(
    
    # Application title
    titlePanel(title="TEST - Student Characteristics", windowTitle = "TEST - Student Characteristics"),
    
    
    fluidRow(
        #cohort
        column(2,
               
               wellPanel(
                   h3("This app explores student characteristics via association rules."),
                   h4("Select Tiers"),
                   selectInput(inputId = "tiers", label = "Choose Tiers", choices =  1:5, selected = 4, multiple =TRUE),
                   br()
                   
               ),
               
               # tier1
               wellPanel(
                   
                   h4("Select Parameter"),
                   br("Support: proportion of students information in the data"),
                   br("Confidence: likelihood that RHS (right-hand-side) depends on LHS (left-hand-side)"),
                   br("Lift: strength of association"),
                   br(),
                   div(),
                   #count
                   sliderInput(
                       inputId = "supp",
                       label = "Choose a minimum support",
                       min = 0.01,
                       max = 0.1, # max 10% of dataset
                       value = 0.05,
                       step = 0.01
                   ),
                   #amount
                   
                   sliderInput(
                       inputId = "conf",
                       label = "Choose a minimum confidence",
                       min = 0.4,
                       max = 0.9,
                       value = 0.6,
                       step = 0.1
                   )
                    
                   
               ),
               wellPanel(
                   
                   #condition = 
                   actionButton(
                       inputId = "simulate", label = "Run", class = "btn btn-success action-button disabled"),
                   br(),
                   tags$br(),
                   tags$button("Restart", id = "restart", type= "button", class = "btn btn-danger action-button", onclick="history.go(0)" )
               )
               
        ), # sidebar col
        
        #right side
        column(10,
               
               fluidRow(
                   column(12,
                          
                          tabsetPanel(
                              # fin apr
                              tabPanel("Explore Student Characteristics",
                                       h5("Frequency plot"),
                                       br(),
                                       div(plotOutput("freqplot")),
                                       br(),
                                       h5("Find interesting relationships using Association rules"),
                                       div(DT::DTOutput("rulesDF"), style = "font-size:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;")
                                       
                              ),
                              #table tab
                              tabPanel("Data Description & Summary Table",
                                        br("FTIC who were FL residents and graduated tri-county high schools"),
                                        br("Cohort: from 2017 to 2021"),
                                        br("Included application month, entry program, demographic, financial, and familial factors"),
                                        br("Finacial aid type: awared merits (UWF funds) and Bright Future (FL funds)"),
                                        h3("Summary Table"),
                                        div(gt_output("gtsummarytable"))
                              )
                          )#tabasetpanel
                   ) # col
               ) 
        ) # rightside end
    )
) #end ui

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   filtered_df <-  reactive({
    
    tricounty <- c("Escambia", "Santa Rosa", "Okaloosa")
    
    selected_data <- student_character_1stFall_df  %>%  
        filter(FTIC_FeeResidency == "Florida Resident (USA)" & HS_CNTY %in% tricounty) %>% #filtered data
        mutate(App_Month = format(as.Date(APP_DT), "%m")) %>% 
        mutate(Age = ifelse( FTIC_Age <= 20, "Under 20","Over 21")) %>% 
        mutate(Awared_PELL = ifelse(PELLGrant == 0, "No", "Yes")) %>% 
        mutate(AidNotPellMerit = TotalFinAid_Term -PELLGrant-ScholarshipsInstitutionalTypes) %>% 
        mutate(TotalAid_NotPellorMerit = ifelse(AidNotPellMerit <= 0, "No", "Yes")) %>% 
        mutate(OtherFinAidSupport = ifelse(AidNotPellMerit == 0 , "NoAid", ifelse(AidNotPellMerit <= 3000, "Below$3,000", "Above$3,000"))) %>% 
        mutate(Pell_Amount = ifelse(PELLGrant == 0, "NoPell",  ifelse(PELLGrant <= 3000, "Below$3,000", "Above$3,000"))) %>%
        mutate(Awarded_Loan = ifelse(LoanTypes == 0, "No", "Yes" )) %>% 
        mutate(HS_NAME = ifelse(is.na(HS_NAME), "Not available", HS_NAME)) %>% 
        mutate(First_Generation = ifelse(Stu_FirstGenInd =="N", "No","Yes")) %>% 
        mutate(Merit_Scholarships = ifelse(ScholarshipsInstitutionalTypes== 0, "No","Yes")) %>% 
        mutate(BF_Scholarships = ifelse(ScholarshipsStateBFTypes == 0, "No","Yes")) %>%  
        filter(APPLICANT_TIER %in% input$tiers  ) %>% #filter tiers
        mutate_if(is.character, as.factor) %>% 
        select("HS_Name"=HS_NAME, 
               App_Month,
               #"Ethnicity"=FTIC_Ethnicity, 
               "Gender"=FTIC_Gender,
               Merit_Scholarships,
               BF_Scholarships,
               "Entry_Program"=FTIC_ProgramCIPDesc,
               #Awared_PELL,
               #TotalAid_NotPellorMerit,
               First_Generation, 
               FTIC_Cohort) # remove cohort for freq.plot
    
    
    
   })
   
 output$freqplot <- renderPlot({
     # import dataset
     ruledata <- as(filtered_df()[,-8], "transactions")
     
     itemFrequencyPlot( ruledata, topN = 20, main="Top 20 Frequency Plot", type="absolute", col = brewer.pal(8, 'Pastel2'))
     
     
 })

 rules <- reactive({

     # import data set
     ruledata <- as(filtered_df()[, -8], "transactions")
  
     #myruledata <- read.transactions("writedata1", sep=",", rm.duplicates = TRUE)
     
     rules_t1 <- apriori( data= ruledata, 
                          parameter = list(support = as.numeric(input$supp), confidence = as.numeric(input$conf), minlen=3, maxlen=5) ) 


 })
       
     
    
    
    output$rulesDF  <- renderDT({
 
         no_redundant_rules <- rules()[!is.redundant(rules())]
         only_sig_rules <- no_redundant_rules[!is.significant(no_redundant_rules),]
         filterd_rules <- subset(only_sig_rules, subset = lift > 1.1)
         outrules <- inspectDT(sort(filterd_rules, by="lift"))


    })
    
    output$gtsummarytable <- render_gt({
        
       tabsum <-  filtered_df() %>% 
            mutate(FTIC_Cohort = as.factor(FTIC_Cohort)) 
       
       tabsum %>% tbl_summary( by = FTIC_Cohort,
                         statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                         missing = "no") %>%  add_n() %>% as_gt()
        
        
    })
    
 
}

# Run the application 
shinyApp(ui = ui, server = server)
