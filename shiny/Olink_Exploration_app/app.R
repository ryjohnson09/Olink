# Olink Expression App

#!#!#!#!#!#!#!#!#!#!#!#!#!#!
# TO DO
# make categories for pathogen, severity and watery vs. dysentery/febrile

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

protein_choices <- setdiff(unique(olink$protein), c("BDNF"))

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
                 
                 # Febrile vs AWS
                 selectInput("diarrhea_type", "Diarrhea Type:", choices = c("Both", "Febrile", "AWD"), selected = "Both"),
                 
                 # Visit
                 checkboxGroupInput('Visit_Number', 'Visit:', choices = visit_choices, 
                                    selected = c("Visit_1", "Visit_4", "Visit_5"), inline = TRUE),
                 helpText("Select patient samples from specified visit number"),
                 
                 # Pathogen Detection
                 selectInput("path_detection", "Pathogen Detection Method", choices = detection_choices, selected = "Both"),
                 helpText("How are samples determined to be positive for pathogen"),
                 
                 # Pathogen Choices
                 uiOutput("secondSelection"),
                 helpText("If", code("All"), ", then all samples included.")))),
      
      ################
      ### Proteins ###
      ################
      fluidRow(
        h3("Proteins"),
        
        column(12, 
               wellPanel(
                 # Proteins of Interest
                 radioButtons("proteins", "Proteins of interest:", choices = c("All", "Select Proteins"), selected = "All"),
                 # Protein output
                 uiOutput("protein_list")))),
      
      #######################
      ### Plot Aesthetics ###
      #######################
      
      fluidRow(
        h3("Ordination and Aesthetics"),
        
        column(12, 
               wellPanel(
                 # X-axis
                 selectInput("xaxis", 
                             label = h4("X-Axis"),
                             choices = c("visit", "LLS_severity", "Diarrhea_classification"), 
                             selected = "visit"),
                 # Point color
                 selectInput("pointColor",
                             label = h4("Point Color"),
                             choices = c("NULL", "visit", "LLS_severity", "Diarrhea_classification"),
                             selected = "NULL"),
                 # axis title size
                 sliderInput("titleSize", 
                             label = h4("Title Size"),
                             min = 1, max = 25, value = 15),
                 # axis text size
                 sliderInput("labelSize", 
                             label = h4("Axis Label Size"),
                             min = 1, max = 25, value = 10),
                 # point alpha
                 sliderInput("pointAlpha", 
                             label = h4("Point Transparency"),
                             min = 0.1, max = 1, value = 0.8),
                 # point size
                 sliderInput("pointSize", 
                             label = h4("Point Size"),
                             min = 1, max = 15, value = 2))),
      
      
        br(),
        downloadButton('downloadPlot','Download Plot')),
      
      
      #sidebar width
      width = 4),
    
    # Plot
    mainPanel(
      plotOutput("plot", width = "800px", height = "800px"),
      
      # Table to see patients (not needed, but useful for troubleshooting)
      fluidRow(column(12,tableOutput('table')))
  )))





## Server ----------------------------------------------------------------
server <- function(input, output) {
  
   
  ##############################################
  ### Filter TrEAT for Patients not in Olink ###
  ##############################################
  
  treat <- treat %>%
    filter(STUDY_ID %in% olink$subject_ID)
  
  
  ###################################
  ### Filter out missing proteins ###
  ###################################
  
  olink <- olink %>%
    filter(protein != "BDNF")
  
  ################################
  ### Matched Samples by Visit ###
  ################################
  
  olink_matched <- reactive({
    if(input$matched == "matched_samples"){
      matched_samples(olink, input$Visit_Number)
      
    } else {
      olink %>%
        filter(visit %in% input$Visit_Number)
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
  
  ###################################
  ### Merge treat to olink tibble ###
  ###################################
  
  treat_olink <- reactive({
    
    olink_matched_protein() %>%
      left_join(., treat, by =c("subject_ID" = "STUDY_ID"))
  })
  
  
  #######################
  ### Treatment Group ###
  #######################
  
  # Filter for treatment Group
  treat_tx <- reactive({
    if(input$tx_group == "AZI"){
      filter(treat_olink(), Treatment == "AZI")
      
    } else if (input$tx_group == "RIF"){
      filter(treat_olink(), Treatment == "RIF")
      
    } else if (input$tx_group == "LEV"){
      filter(treat_olink(), Treatment == "LEV")
      
    } else {
      return(treat_olink())
    }
  })
  
  
  ##############################
  ### Filter by Disease Type ###
  ##############################
  treat_tx_disease <- reactive({
    if(input$diarrhea_type == "Febrile"){
      filter(treat_tx(), Diarrhea_classification == "Febrile")
      
    } else if (input$diarrhea_type == "AWD"){
      filter(treat_tx(), Diarrhea_classification == "AWD")
      
    } else if (input$diarrhea_type == "Both"){
      treat_tx()
      
    } else {
      stopApp("Error when filtering by Disease Type")
    }
  })
  
  
  #######################################
  ### Render pathogen detection list ###
  #######################################
  output$secondSelection <- renderUI({
    if(input$path_detection == "Culture"){
      selectInput("pathogens", "Pathogens", choices = culture_choices)
    } else if (input$path_detection == "Taq"){
      selectInput("pathogens", "Pathogens", choices = taq_choices)
    } else if (input$path_detection == "Either"){
      selectInput("pathogens", "Pathogens", choices = either_choices)
    } else if (input$path_detection == "Both") {
      selectInput("pathogens", "Pathogens", choices = both_choices)
    } else {
      stopApp("Problem when rendering pathogen list")
    }
  })
  
  
  ##########################
  ### Filter by Pathogen ###
  ##########################
  
  # Filter for pathogen of interest
  treat_tx_pathogen <- reactive({
    
    # Include all patients
    if (input$pathogens == "All"){
      treat_tx_disease()
    } else {
      
      # Select for patients with specified pathogen
      pathogen <- sym(input$pathogens)
      treat_tx_disease() %>%
        filter(!!pathogen == "yes")
    }
  })
  
  
  
  ##############################
  #### Create plots/tables #####
  ##############################
  
  plotInput <- reactive({
    
    ggplot(data = treat_tx_pathogen(), aes_string(
      
      # graph aes
      x = input$xaxis, 
      y = "olink_value",
      color = input$pointColor)) +
      
      geom_jitter(height = 0, width = 0.1, 
                  size = input$pointSize, 
                  alpha = input$pointAlpha) +
      geom_hline(aes(yintercept = LOD_value), linetype = "dashed") +
      facet_wrap(~protein) +
      theme(
        axis.text.x = element_text(size = input$labelSize, angle = 30, hjust = 1),
        axis.text.y = element_text(size = input$labelSize),
        axis.title.x = element_text(size = input$titleSize),
        axis.title.y = element_text(size = input$titleSize),
        strip.text.x = element_text(size = input$titleSize)
      )
      
  })
  
  
  ####################
  ### Display Plot ###
  ####################
  output$plot <- renderPlot({
    print(plotInput())
  })
  

  
  #####################
  ### Download plot ###
  #####################
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("shiny_plot",'.png',sep='')},
    content = function(file){
      ggsave(file, plot=plotInput())
    })
  
  
  #output$table <- renderTable({head(treat_tx(), 50)})
  
}




## Run the application ---------------------------------------------------
shinyApp(ui = ui, server = server)

