# Olink Expression App

library(shiny)
library(tidyverse)

########################
### Source Functions ###
########################
source("Olink_Exploration_Functions.R")

#######################
#### Read in Files ####
#######################

treat <- suppressWarnings(suppressMessages(read_csv("TrEAT_Clinical_Metadata_tidy.csv")))
olink <- suppressWarnings(suppressMessages(read_csv("Olink_tidy.csv")))


######################
### Set Up Choices ###
######################

matched_choices <- list("All Samples" = "all_samples",
                        "Matched Samples" = "matched_samples")

visit_choices <- c("Visit_1", "Visit_4", "Visit_5")

tx_choices <- c("RIF", "LEV", "AZI", "All")

protein_choices <- unique(olink$protein)

detection_choices <- c("Culture", "Taq", "Both", "Either")

culture_choices <- c("All", colnames(treat)[grep(pattern = "_culture$", colnames(treat))])

taq_choices <- c("All", colnames(treat)[grep(pattern = "_taq$", colnames(treat))])

both_choices <- c("All", colnames(treat)[grep(pattern = "_both$", colnames(treat))])

either_choices <- c("All", colnames(treat)[grep(pattern = "_either$", colnames(treat))])



## UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Olink Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      
      # Matched or All samples
      radioButtons("matched", label = "Matched or All samples",
                   choices = matched_choices, inline = TRUE, selected = "matched_samples"),
      helpText("Matched = Only patients that provided samples for all selected visits"),
      
      ##########################################
      ### Inputs that Select Certain Samples ###
      ##########################################
      fluidRow(
        h3("Sample Selection:"),
        
        column(12, 
               wellPanel(
                 # Treatment Choices
                 selectInput("tx_group", "Treatment Groups", choices = tx_choices, selected = "All"),
                 helpText("Select patients samples in specified treatment groups"),
                 
                 # Visit
                 checkboxGroupInput('Visit_Number', 'Visit:', choices = visit_choices, selected = c(1, 4, 5), inline = TRUE),
                 helpText("Select patient samples from specified visit number"),
                 
                 # Pathogen Detection
                 selectInput("path_detection", "Pathogen Detection Method", choices = detection_choices, selected = "Both"),
                 helpText("How are samples determined to be positive for pathogen"),
                 
                 # Pathogen Choices
                 uiOutput("secondSelection"),
                 helpText("If", code("All"), ", then all samples included.")))),
      
      fluidRow(
        h3("Proteins"),
        
        column(12, 
               wellPanel(
                 # Proteins of Interest
                 radioButtons("proteins", "Proteins of interest:", choices = c("All", "Select Proteins"), selected = "All"),
                 # Protein output
                 uiOutput("protein_list"),
                 helpText("Select proteins of interest")))),
      
      #sidebar width
      width = 4),
    
    # Plot
    mainPanel(
      plotOutput("plot", width = "800px", height = "800px")
      
      # Table to see patients (not needed, but useful for troubleshooting)
      #fluidRow(column(12,tableOutput('table'))),
  )))





## Server ----------------------------------------------------------------
server <- function(input, output) {
  
   
  ##############################################
  ### Filter TrEAT for Patients not in Olink ###
  ##############################################
  
  treat <- treat %>%
    filter(STUDY_ID %in% olink$subject_ID)
  
  #########################################
  ### Remove samples from humichip that ###
  ### are in the LOP & PLA tx groups ######
  #########################################
  
  LOP_PLA_samples <- treat %>%
    filter(Treatment %in% c("LOP", "PLA")) %>%
    pull(STUDY_ID)
  
  olink <- olink %>%
    filter(!subject_ID %in% LOP_PLA_samples)
  
  rm(LOP_PLA_samples)
  
  
  ################################
  ### Matched Samples by Visit ###
  ################################
  
  olink_matched <- reactive({
    if(input$matched == "matched_samples"){
      matched_choices(olink, input$Visit_Number)
      
    } else {
      olink
    }
  })
  
  #########################
  ### Filter by Protein ###
  #########################
  
  # Generate list of proteins if needed
  output$protein_list <- renderUI({
    if (input$proteins != "All"){
      checkboxGroupInput("protein_list", "Proteins:", 
                         choices = protein_choices, 
                         selected = "All", 
                         inline = TRUE)
    }
  })
  
  # Filter Olink for proteins of interest
  olink_matched_protein <- reactive({
    if(input$proteins == "All"){
      olink_matched()
    } else if (input$proteins != "All"){
      olink_matched() %>%
        filter(protein %in% input$protein_list)
    } else {
      stopApp("Error at protein filtering")
    }
  })
  
  
  #######################
  ### Treatment Group ###
  #######################
  
  # Filter for treatment Group
  treat_tx <- reactive({
    if(input$tx_group == "AZI"){
      filter(treat, Treatment == "AZI")
      
    } else if (input$tx_group == "RIF"){
      filter(treat, Treatment == "RIF")
      
    } else if (input$tx_group == "LEV"){
      filter(treat, Treatment == "LEV")
      
    } else {
      return(treat)
    }
  })
  
  
  ##########################
  ### Filter by Pathogen ###
  ##########################
  
  # Filter for pathogen of interest
  treat_tx_pathogen <- reactive({
    
    # Include all patients
    if (input$pathogens == "All"){
      treat_tx()
    } else {
      
      # Select for patients with specified pathogen
      pathogen <- sym(input$pathogens)
      treat_tx() %>%
        filter(!!pathogen == "yes")
    }
  })
  
  

  
  
  
}




## Run the application ---------------------------------------------------
shinyApp(ui = ui, server = server)

